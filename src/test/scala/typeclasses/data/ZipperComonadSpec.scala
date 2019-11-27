package typeclasses.data

import org.scalatest._
import typeclasses.syntax.zipper._

class ZipperComonadSpec extends FlatSpec with Matchers {

  val initialZipper: Zipper[Int] = Zipper(Stream(4,3,2,1), 5, Stream(6,7,8))

  "ZipperComonad" should "have a valid extract" in {
    initialZipper.extract shouldBe 5
  }
  it should "have a valid duplicate" in {
    val smallZipper: Zipper[Int] = Zipper(Stream(1), 2, Stream(3))

    val expected = Zipper(
      Stream(Zipper(Stream.empty, 1, Stream(2,3))),
      smallZipper,
      Stream(Zipper(Stream(2,1), 3, Stream.empty))
    )
    smallZipper.duplicate shouldBe expected
  }
  it should "have valid coflatMap" in {
    def sumLeftRight(streamZ: Zipper[Int]): Int = {
      streamZ.focus + streamZ.moveLeft.focus + streamZ.moveRight.focus
    }
    // Note: the leftmost and rightmost values lack an identity element for what to do in the case where the stream is empty
    initialZipper.coflatMap(sumLeftRight).toList shouldBe List(4,6,9,12,15,18,21,23)
  }
  it should "have a valid map" in {
    initialZipper.map(_.toString).toStream shouldBe Stream("1", "2", "3", "4", "5", "6", "7", "8")
  }
}