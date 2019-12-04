package typeclasses.data

import org.scalatest._

class ZipperSpec extends FlatSpec with Matchers {

  val initialZipper: Zipper[Int] = Zipper(Stream(4,3,2,1).map(Option(_)), Option(5), Stream(6,7,8).map(Option(_)))

  "Zipper" should "have valid focus" in {
    initialZipper.focus shouldBe Some(5)
  }
  it should "shift focus left" in {
    initialZipper.moveLeft shouldBe Zipper(Stream(3,2,1).map(Option(_)), Option(4), Stream(5,6,7,8).map(Option(_)))
  }
  it should "stop shifting left when no more lefts present" in {
    val leftEmpty = Zipper(Stream.empty, Some(1), Stream(Some(2)))
    leftEmpty.moveLeft shouldBe Zipper(Stream.empty, None, Stream(Some(1), Some(2)))
  }
  it should "shift focus right" in {
    initialZipper.moveRight shouldBe Zipper(Stream(5,4,3,2,1).map(Option(_)), Option(6), Stream(7,8).map(Option(_)))
  }
  it should "stop shifting right when no more right's present" in {
    val rightEmpty = Zipper(Stream(Some(2)), Some(1), Stream.empty)
    rightEmpty.moveRight shouldBe Zipper(Stream(Some(1), Some(2)), None, Stream.empty)
  }
  it should "create a list from the zipper intuitively" in {
    initialZipper.toList shouldBe List(1,2,3,4,5,6,7,8)
  }
  it should "duplicateLeft" in {
    val smallerZipper: Zipper[Int] = Zipper(Stream(2,1).map(Option(_)), Some(3), Stream(Some(4)))

    smallerZipper.duplicateLeft(identity).toList shouldBe List(
      Zipper(Stream(Some(1)), Some(2), Stream(3,4).map(Option(_))),
      Zipper(Stream(), Some(1), Stream(2,3,4).map(Option(_)))
    ).map(Option(_))
  }
  it should "duplicateRight" in {
    val smallerZipper: Zipper[Int] = Zipper(Stream(Some(1)), Some(2), Stream(3,4).map(Option(_)))
    smallerZipper.duplicateRight(identity).toList shouldBe List(
      Zipper(Stream(2,1).map(Option(_)), Some(3), Stream(Some(4))),
      Zipper(Stream(3,2,1).map(Option(_)),Some(4),Stream())
    ).map(Option(_))
  }
}
