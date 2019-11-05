case class StreamZipper[A](left: Stream[A], focus: A, right: Stream[A]) {
  // you kind of have to disregard the directionality of the stream here, but that's ok
  def moveRight: StreamZipper[A] = {
    StreamZipper(focus #:: left , right.head, right.tail)
  }

  def moveLeft: StreamZipper[A] = {
    StreamZipper(left.tail, left.head, focus #:: right)
  }

  def prettyPrint: String = {
    val leftValues = this.left.toList.reverse.mkString(", ")
    val focus = s" (${this.focus}) "
    val rightValues = this.right.toList.mkString(", ")
    leftValues ++ focus ++ rightValues
  }

  // TODO: Generalize these
  def streamRights: Stream[StreamZipper[A]] = Stream.iterate(this)(_.moveRight)
    .tail
    .zip(right)
    .map(_._1)

  def streamLefts: Stream[StreamZipper[A]] = Stream.iterate(this)(_.moveLeft)
    .tail
    .zip(left)
    .map(_._1)

  def streamRF[B](f:StreamZipper[A] => B): Stream[B] = Stream.iterate(this)(_.moveRight)
    .tail
    .zip(right)
    .map(t => f(t._1))

  def streamLF[B](f:StreamZipper[A] => B): Stream[B] = Stream.iterate(this)(_.moveLeft)
    .tail
    .zip(left)
    .map(t => f(t._1))

}

object StreamZipperOps {
  implicit def zipperComonad: Comonad[StreamZipper] = new Comonad[StreamZipper] {
    override def counit[A](w: StreamZipper[A]): A = w.focus
    override def cojoin[A](w: StreamZipper[A]): StreamZipper[StreamZipper[A]] = {
      StreamZipper(w.streamLefts, w, w.streamRights)
    }
    override def coflatMap[A, B](w: StreamZipper[A])(f: StreamZipper[A] => B): StreamZipper[B] = {
      StreamZipper(w.streamLF(f), f(w.focus), w.streamRF(f))
    }
  }
}

object test extends App {

  import StreamZipperOps._

  val testZip = StreamZipper(Stream(4,3,2,1), 5, Stream(6,7,8))
  println(testZip.prettyPrint)
  println(testZip.moveLeft.prettyPrint)
  println(testZip.moveRight.prettyPrint)
}
