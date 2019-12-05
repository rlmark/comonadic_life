package conway.data

import org.scalatest._

class ZipperSpec extends FlatSpec with Matchers {

  val initialZipper: Zipper[Int] = Zipper(LazyList(4,3,2,1), 5, LazyList(6,7,8))

  "Zipper" should "have valid focus" in {
    initialZipper.focus shouldBe 5
  }
  it should "shift focus left" in {
    initialZipper.moveLeft shouldBe Zipper(LazyList(3,2,1), 4, LazyList(5,6,7,8))
  }
  it should "stop shifting left when no more lefts present" in {
    val leftEmpty = Zipper(LazyList.empty, 1, LazyList(2,3))
    leftEmpty.moveLeft shouldBe leftEmpty
  }
  it should "shift focus right" in {
    initialZipper.moveRight shouldBe Zipper(LazyList(5,4,3,2,1), 6, LazyList(7,8))
  }
  it should "stop shifting right when no more right's present" in {
    val rightEmpty = Zipper(LazyList(2), 1, LazyList.empty)
    rightEmpty.moveRight shouldBe rightEmpty
  }
  it should "create a list from the zipper intuitively" in {
    initialZipper.toList shouldBe List(1,2,3,4,5,6,7,8)
  }
  it should "duplicateLeft" in {
    val smallerZipper: Zipper[Int] = Zipper(LazyList(2,1), 3, LazyList(4))

    smallerZipper.duplicateLeft(identity).toList shouldBe List(
      Zipper(LazyList(1), 2, LazyList(3,4)),
      Zipper(LazyList(), 1, LazyList(2,3,4))
    )
  }
  it should "duplicateRight" in {
    val smallerZipper: Zipper[Int] = Zipper(LazyList(1), 2, LazyList(3,4))
    smallerZipper.duplicateRight(identity).toList shouldBe List(
      Zipper(LazyList(2,1), 3, LazyList(4)),
      Zipper(LazyList(3,2,1),4,LazyList())
    )
  }
}
