package instances

import org.scalatest._
import StreamZipperComonad._
import syntax.streamZipper._

class StreamZipperComonadSpec extends FlatSpec with Matchers {

  val initialZipper: StreamZipper[Int] = StreamZipper(Stream(4,3,2,1), 5, Stream(6,7,8))

  "StreamZipperComonad" should "have a valid counit" in {
    initialZipper.counit shouldBe 5
  }
  it should "have a valid coflatten" in {
    val smallZipper: StreamZipper[Int] = StreamZipper(Stream(1), 2, Stream(3))

    val expected = StreamZipper(
      Stream(StreamZipper(Stream.empty, 1, Stream(2,3))),
      smallZipper,
      Stream(StreamZipper(Stream(2,1), 3, Stream.empty))
    )
    smallZipper.coflatten shouldBe expected
  }
  it should "have valid coflatMap" in {
    def sumLeftRight(streamZ: StreamZipper[Int]): Int = {
      streamZ.focus + streamZ.moveLeft.focus + streamZ.moveRight.focus
    }
    // Note: the leftmost and rightmost values lack an identity element for what to do in the case where the stream is empty
    initialZipper.coflatMap(sumLeftRight).toList shouldBe List(4,6,9,12,15,18,21,23)
  }
}
