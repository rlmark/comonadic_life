package catlike.data

import catlike.Comonad

case class StreamZipper[A](left: Stream[A], focus: A, right: Stream[A]) {
  def setFocus(a: A): StreamZipper[A] = {
    this.copy(focus = a)
  }

  def moveRight: StreamZipper[A] = {
    if (right.isEmpty) this
    else StreamZipper(focus #:: left , right.head, right.tail)
  }

  // Maybe these should have an implicit monoid instance of A so we know what to do when it's empty.
  def moveLeft: StreamZipper[A] = {
    if (left.isEmpty) this // This might become a problem
    else StreamZipper(left.tail, left.head, focus #:: right)
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

  def duplicateRight[B](f:StreamZipper[A] => B): Stream[B] =
    Stream.iterate(this)(_.moveRight)
    .tail
    .zip(right)
    .map(t => f(t._1))

  def duplicateLeft[B](f:StreamZipper[A] => B): Stream[B] =
    Stream.iterate(this)(_.moveLeft)
    .tail
    .zip(left)
    .map(t => f(t._1))

}

object StreamZipper extends App  {

  def fromList[A](items: List[A]): StreamZipper[A] = {
    val left = items.take(items.size)
    StreamZipper(left.tail.toStream, left.head, Stream.empty)
  }

  implicit def zipperComonad: Comonad[StreamZipper] = new Comonad[StreamZipper] {
    override def extract[A](w: StreamZipper[A]): A = w.focus

    override def duplicate[A](w: StreamZipper[A]): StreamZipper[StreamZipper[A]] = {
      StreamZipper(w.duplicateLeft(identity), w, w.duplicateRight(identity))
    }

    override def map[A, B](fa: StreamZipper[A])(f: A => B): StreamZipper[B] = {
      StreamZipper(fa.left.map(f) ,f(fa.focus), fa.right.map(f))
    }
  }
}
