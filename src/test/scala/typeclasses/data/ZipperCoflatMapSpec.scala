package typeclasses.data

import org.scalatest._
import typeclasses.syntax.zipper._

class ZipperCoflatMapSpec extends FlatSpec with Matchers {

  val initialZipper: Zipper[Int] = Zipper(Stream(4,3,2,1).map(Option(_)), Some(5), Stream(6,7,8).map(Option(_)))

  "ZipperCoflatMap" should "have a valid duplicate" in {
    val smallZipper: Zipper[Int] = Zipper(Stream(Some(1)), Some(2), Stream(Some(3)))

    val expected = Zipper(
      Stream(Some(Zipper(Stream.empty, Some(1), Stream(2,3).map(Option(_))))),
      Some(smallZipper),
      Stream(Some(Zipper(Stream(2,1).map(Option(_)), Some(3), Stream.empty)))
    )
    smallZipper.duplicate shouldBe expected
  }
  it should "have associativity" in {
    initialZipper.duplicate.duplicate shouldBe initialZipper.coflatMap(_.duplicate)
  }
  it should "have valid coflatMap" in {
    def sumLeftRight(streamZ: Zipper[Int]): Int = {
      streamZ.focus.getOrElse(0) + streamZ.moveLeft.focus.getOrElse(0) + streamZ.moveRight.focus.getOrElse(0)
    }
    initialZipper.coflatMap(sumLeftRight).toList shouldBe List(3,6,9,12,15,18,21,15)
  }
  it should "have a valid map" in {
    initialZipper.map(_.toString).toList shouldBe List("1", "2", "3", "4", "5", "6", "7", "8")
  }
}
