package conway.data

import org.scalatest._
import cats.syntax.comonad._
import cats.syntax.coflatMap._
import cats.syntax.functor._

class ZipperComonadSpec extends FlatSpec with Matchers {

  val initialZipper: Zipper[Int] = Zipper(LazyList(4, 3, 2, 1), 5, LazyList(6, 7, 8))

  "ZipperComonad" should "have a valid extract" in {
    initialZipper.extract shouldBe 5
  }
  it should "have a valid duplicate" in {
    val smallZipper = Zipper(LazyList(1), 2, LazyList(3))
    val expected = Zipper(
      LazyList(Zipper(LazyList.empty, 1, LazyList(2, 3))),
      smallZipper,
      LazyList(Zipper(LazyList(2, 1), 3, LazyList.empty))
    )
    smallZipper.coflatten shouldBe expected
  }
  it should "have valid right identity" in {
    initialZipper.coflatMap(_.extract) shouldBe initialZipper
  }
  it should "have valid left identity" in {
    initialZipper.coflatten.extract shouldBe initialZipper
  }
  it should "have associativity" in {
    initialZipper.coflatten.coflatten shouldBe initialZipper.coflatMap(_.coflatten)
  }
  it should "have valid coflatMap" in {
    def sumLeftRight(z: Zipper[Int]): Int =
      z.focus + z.moveLeft.focus + z.moveRight.focus

    initialZipper.coflatMap(sumLeftRight).toList shouldBe List(4, 6, 9, 12, 15, 18, 21, 23)
  }
  it should "have a valid map" in {
    initialZipper.map(_.toString).toList shouldBe List("1", "2", "3", "4", "5", "6", "7", "8")
  }
}
