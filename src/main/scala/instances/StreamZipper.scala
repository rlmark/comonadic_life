package instances

case class StreamZipper[A](left: Stream[A], focus: A, right: Stream[A]) {
  // you kind of have to disregard the directionality of the stream for the leftt, but that's ok
  def moveRight: StreamZipper[A] = {
    if (right.isEmpty) this else {
      StreamZipper(focus #:: left , right.head, right.tail)
    }
  }

  def moveLeft: StreamZipper[A] = {
    if (left.isEmpty) this
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
