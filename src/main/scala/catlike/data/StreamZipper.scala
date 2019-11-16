package catlike.data

import catlike.Comonad

case class StreamZipper[A](left: Stream[A], focus: A, right: Stream[A]) {
  // you kind of have to disregard the directionality of the stream for the left, but that's ok
  def moveRight: StreamZipper[A] = {
    if (right.isEmpty) this else {
      StreamZipper(focus #:: left , right.head, right.tail)
    }
  }

  // Maybe these should return options.
  def moveLeft: StreamZipper[A] = {
    if (left.isEmpty) this // This is quickly becoming a problem
    else StreamZipper(left.tail, left.head, focus #:: right)
  }

  def prettyPrint: String = {
    val leftValues = this.left.toList.reverse.mkString(", ")
    val focus = s" (${this.focus}) "
    val rightValues = this.right.toList.mkString(", ")
    leftValues ++ focus ++ rightValues
  }

  def toList: List[A] = {
    left.toList.reverse ++ (focus +: right.toList)
  }

  def toStream: Stream[A] = {
    left.reverse #::: (focus #:: right)
  }

  def streamRightF[B](f:StreamZipper[A] => B): Stream[B] =
    Stream.iterate(this)(_.moveRight)
    .tail
    .zip(right)
    .map(t => f(t._1))

  def streamLeftF[B](f:StreamZipper[A] => B): Stream[B] =
    Stream.iterate(this)(_.moveLeft)
    .tail
    .zip(left)
    .map(t => f(t._1))

}

object StreamZipper {
  implicit def zipperComonad: Comonad[StreamZipper] = new Comonad[StreamZipper] {
    override def extract[A](w: StreamZipper[A]): A = w.focus

    override def duplicate[A](w: StreamZipper[A]): StreamZipper[StreamZipper[A]] = {
      StreamZipper(w.streamLeftF(identity), w, w.streamRightF(identity))
    }

    override def coflatMap[A, B](w: StreamZipper[A])(f: StreamZipper[A] => B): StreamZipper[B] = {
      // Alternatively: StreamZipper(w.streamLeftF(f), f(w), w.streamRightF(f))
      map(duplicate(w))(f)
    }

    override def map[A, B](fa: StreamZipper[A])(f: A => B): StreamZipper[B] = {
      StreamZipper(fa.left.map(f) ,f(fa.focus), fa.right.map(f))
    }
  }
}
