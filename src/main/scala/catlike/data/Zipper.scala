package catlike.data

import catlike.Comonad

case class Zipper[A](left: Stream[A], focus: A, right: Stream[A]) {
  def setFocus(a: A): Zipper[A] = {
    this.copy(focus = a)
  }

  def moveRight: Zipper[A] = {
    if (right.isEmpty) this
    else Zipper(focus #:: left , right.head, right.tail)
  }

  // Maybe these should have an implicit monoid instance of A so we know what to do when it's empty.
  def moveLeft: Zipper[A] = {
    if (left.isEmpty) this // This might become a problem
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

  def duplicateRight[B](f:Zipper[A] => B): Stream[B] =
    Stream.iterate(this)(_.moveRight)
    .tail
    .zip(right)
    .map(t => f(t._1))

  def duplicateLeft[B](f:Zipper[A] => B): Stream[B] =
    Stream.iterate(this)(_.moveLeft)
    .tail
    .zip(left)
    .map(t => f(t._1))

}

object Zipper {
  def fromList[A](items: List[A]): Zipper[A] = {
    // Will throw if items is empty, so beware!
    Zipper(items.tail.toStream, items.head, Stream.empty)
  }

  implicit def zipperComonad: Comonad[Zipper] = new Comonad[Zipper] {
    override def extract[A](w: Zipper[A]): A = w.focus

    override def duplicate[A](w: Zipper[A]): Zipper[Zipper[A]] = {
      Zipper(w.duplicateLeft(identity), w, w.duplicateRight(identity))
    }

    override def map[A, B](fa: Zipper[A])(f: A => B): Zipper[B] = {
      Zipper(fa.left.map(f) ,f(fa.focus), fa.right.map(f))
    }
  }
}
