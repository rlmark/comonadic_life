package conway.data

import cats.{Comonad, Eq}
import cats.instances.lazyList._
import conway.data.Zipper.unfold

case class Zipper[A](left: LazyList[A], focus: A, right: LazyList[A]) {

  def maybeRight: Option[Zipper[A]] = right match {
    case nextRight #:: rights => Some(Zipper(focus #:: left, nextRight, rights))
    case _ => None
  }

  def maybeLeft: Option[Zipper[A]] = left match {
    case nextLeft +: lefts => Some(Zipper(lefts, nextLeft, focus +: right))
    case _ => None
  }

  def setFocus(a: A): Zipper[A] = {
    this.copy(focus = a)
  }

  def moveRight: Zipper[A] = {
    if (right.isEmpty) this
    else Zipper(focus #:: left, right.head, right.tail)
  }

  def moveLeft: Zipper[A] = {
    if (left.isEmpty) this
    else Zipper(left.tail, left.head, focus #:: right)
  }

  def prettyPrint: String = {
    val leftValues = this.left.toList.reverse.mkString(", ")
    val focus = if (left.isEmpty) s"${Console.BLUE}${this.focus}, ${Console.RESET}" else s",${Console.BLUE} ${this.focus}${Console.RESET}, "
    val rightValues = this.right.toList.mkString(", ")
    leftValues ++ focus ++ rightValues
  }

  def toList: List[A] =
    left.toList.reverse ++ (focus +: right.toList)

  def toStream: LazyList[A] =
    left.reverse #::: (focus #:: right)

  def duplicateRight[B](f: Zipper[A] => B): LazyList[B] =
    unfold(this)(z => z.maybeRight.map(x => (f(x), x)))

  def duplicateLeft[B](f: Zipper[A] => B): LazyList[B] =
    unfold(this)(z => z.maybeLeft.map(x => (f(x), x)))

}

object Zipper {
  def fromList[A](items: List[A]): Zipper[A] =
    Zipper(LazyList.from(items.tail), items.head, LazyList.empty)


  def unfold[A, B](a: A)(f: A => Option[(B, A)]): LazyList[B] =
    f(a) match {
      case Some((b, a)) => b #:: unfold(a)(f)
      case None => LazyList.empty
    }


  implicit def zipperEq[A: Eq]: Eq[Zipper[A]] = {
    import Eq._
    and(and(by(_.left),by(_.focus)), by(_.right))
  }

  implicit def zipperComonad: Comonad[Zipper] = new Comonad[Zipper] {
    override def extract[A](w: Zipper[A]): A =
      w.focus

    override def map[A, B](fa: Zipper[A])(f: A => B): Zipper[B] =
      Zipper(fa.left.map(f), f(fa.focus), fa.right.map(f))

    override def coflatMap[A, B](fa: Zipper[A])(f: Zipper[A] => B): Zipper[B] =
      Zipper(fa.duplicateLeft(f), f(fa), fa.duplicateRight(f))
  }
}
