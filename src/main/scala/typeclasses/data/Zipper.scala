package typeclasses.data

import typeclasses.{CoflatMap, Comonad}
import typeclasses.data.Zipper.unfold

case class Zipper[A](left: Stream[Option[A]], focus: Option[A], right: Stream[Option[A]]) {

  def maybeRight: Option[Zipper[A]] = right match {
    case nextRight #:: rights => Some(Zipper(focus #:: left, nextRight, rights))
    case _ => None
  }

  def maybeLeft: Option[Zipper[A]] = left match {
    case nextLeft +: lefts => Some(Zipper(lefts, nextLeft, focus +: right))
    case _ => None
  }

  def setFocus(a: A): Zipper[A] = {
    this.copy(focus = Some(a))
  }

  def moveRight: Zipper[A] = {
    if (right.isEmpty) Zipper(focus #:: left, None, right)
    else Zipper(focus #:: left , right.head, right.tail)
  }

  def moveLeft: Zipper[A] = {
    if (left.isEmpty) Zipper(left, None, focus #:: right)
    else Zipper(left.tail, left.head, focus #:: right)
  }

  def prettyPrint: String = {
    val leftValues = this.left.toList.reverse.mkString(", ")
    val focus = if (left.isEmpty) s"${Console.BLUE}${this.focus}, ${Console.RESET}" else s",${Console.BLUE} ${this.focus}${Console.RESET}, "
    val rightValues = this.right.toList.mkString(", ")
    leftValues ++ focus ++ rightValues
  }

  def toList: List[A] = {
    (left.toList.reverse ++ (focus +: right.toList)).flatten
  }

  def toStream: Stream[Option[A]] = {
    left.reverse #::: (focus #:: right)
  }

  def duplicateRight[B](f:Zipper[A] => B): Stream[Option[B]] =
    unfold(this)(z => z.maybeRight.map(x => (Option(f(x)), x)))

  def duplicateLeft[B](f:Zipper[A] => B): Stream[Option[B]] =
    unfold(this)(z => z.maybeLeft.map(x => (Option(f(x)), x)))

}

object Zipper {
  def fromList[A](items: List[A]): Zipper[A] = {
    // Will throw if items is empty, so beware!
    Zipper(items.tail.toStream.map(Option(_)), Option(items.head), Stream.empty)
  }

  def unfold[A, B](a: A)(f: A => Option[(B, A)]): Stream[B] = f(a) match {
    case Some((b, a)) => b #:: unfold(a)(f)
    case None         => Stream.empty
  }

  implicit def zipperCoflatMap: CoflatMap[Zipper] = new CoflatMap[Zipper] {
    override def duplicate[A](w: Zipper[A]): Zipper[Zipper[A]] = {
        Zipper(w.duplicateLeft(identity), Option(w), w.duplicateRight(identity))
    }

    override def map[A, B](fa: Zipper[A])(f: A => B): Zipper[B] = {
      Zipper(fa.left.map(_.map(f)) , fa.focus.map(f), fa.right.map(_.map(f)))
    }
  }
}
