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
}

object test extends App {
  val testZip = StreamZipper(Stream(4,3,2,1), 5, Stream(6,7,8))
  println(testZip.prettyPrint)
  println(testZip.moveLeft.prettyPrint)
  println(testZip.moveRight.prettyPrint)
}
