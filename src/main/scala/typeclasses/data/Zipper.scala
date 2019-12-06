package typeclasses.data

import typeclasses.Comonad
import typeclasses.data.Zipper.unfold

case class Zipper[A](left: Stream[A], focus: A, right: Stream[A]) {

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
    else Zipper(focus #:: left , right.head, right.tail)
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

  def toList: List[A] = {
    left.toList.reverse ++ (focus +: right.toList)
  }

  def toStream: Stream[A] = {
    left.reverse #::: (focus #:: right)
  }

  def duplicateRights[B]: Stream[Zipper[A]] =
    unfold(this)(z => z.maybeRight.map(x => (x, x)))

  def duplicateLefts[B]: Stream[Zipper[A]] =
    unfold(this)(z => z.maybeLeft.map(x => (x, x)))
}

object Zipper {
  def fromList[A](items: List[A]): Zipper[A] = {
    // Will throw if items is empty, so beware!
    Zipper(items.tail.toStream, items.head, Stream.empty)
  }

  def unfold[A, B](a: A)(f: A => Option[(B, A)]): Stream[B] = f(a) match {
    case Some((b, a)) => b #:: unfold(a)(f)
    case None         => Stream.empty
  }

  implicit def zipperComonad: Comonad[Zipper] = new Comonad[Zipper] {
    override def extract[A](w: Zipper[A]): A = w.focus

    override def duplicate[A](w: Zipper[A]): Zipper[Zipper[A]] = {
        Zipper(w.duplicateLefts, w, w.duplicateRights)
    }

    override def map[A, B](fa: Zipper[A])(f: A => B): Zipper[B] = {
      Zipper(fa.left.map(f) ,f(fa.focus), fa.right.map(f))
    }
  }
}
