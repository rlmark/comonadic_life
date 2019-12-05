package conway.data

import org.scalatest._
import conway.data.Zipper._
import cats.syntax.coflatMap._
import cats.syntax.comonad._


class GridZipperSpec extends FlatSpec with Matchers {

  "extract" should "extract the focus" in {
    val streamZipper = Zipper(LazyList(1), 2, LazyList(3))
    val largeStreamZipper: Zipper[Zipper[Int]] = streamZipper.coflatten
    val gridZipper = GridZipper(largeStreamZipper)

    gridZipper.extract shouldBe 2
  }
  it should "shift left and focus" in {
    val streamZipper = Zipper(LazyList(1), 2, LazyList(3))
    val largeStreamZipper: Zipper[Zipper[Int]] = streamZipper.coflatten
    val gridZipper = GridZipper(largeStreamZipper)

    GridZipper(gridZipper.value.moveLeft).extract shouldBe 1
  }
  "duplicate" should "handle basic case" in {
    val streamZipper = Zipper(LazyList(), 2, LazyList())
    val largeStreamZipper: Zipper[Zipper[Int]] = streamZipper.coflatten
    val gridZipper = GridZipper(largeStreamZipper)

    val streamOfGrid: Zipper[Zipper[GridZipper[Int]]] = Zipper(
      LazyList(),
      Zipper(
        LazyList(),
        gridZipper,
        LazyList()
      ),
      LazyList()
    )
    val expectedGridZipper: GridZipper[GridZipper[Int]] = GridZipper(
      streamOfGrid
    )

    gridZipper.coflatten shouldBe expectedGridZipper
  }
  it should "duplicate more complicated zippers" in pendingUntilFixed {
    val streamZipper = Zipper(LazyList(1), 2, LazyList())
    val largeStreamZipper: Zipper[Zipper[Int]] = streamZipper.coflatten
    val gridZipper = GridZipper(largeStreamZipper)

    val expectedGridZipper: GridZipper[GridZipper[Int]] = GridZipper(
      Zipper(
        LazyList(
          Zipper(
            LazyList(),
            GridZipper(
              Zipper(
                LazyList(),
                Zipper(LazyList(), 1, LazyList(2)),
                LazyList(Zipper(LazyList(1), 2, LazyList()))
              )
            ),
            LazyList(
              GridZipper(
                Zipper(
                  LazyList(Zipper(LazyList(), 1, LazyList(2))),
                  Zipper(LazyList(1), 2, LazyList()),
                  LazyList()
                )
              )
            )
          )
        ),
        Zipper(
          LazyList(
            GridZipper(
              Zipper(
                LazyList(),
                Zipper(LazyList(), 1, LazyList(2)),
                LazyList(Zipper(LazyList(1), 2, LazyList()))
              )
            )
          ),
          GridZipper(
            Zipper(
              LazyList(Zipper(LazyList(), 1, LazyList(2))),
              Zipper(LazyList(1), 2, LazyList()),
              LazyList()
            )
          ),
          LazyList()
        ),
        LazyList()
      )
    )

    gridZipper.coflatten shouldBe expectedGridZipper
  }
  it should "have right identity" in {
    val streamZipper = Zipper(LazyList(), 2, LazyList())
    val largeStreamZipper: Zipper[Zipper[Int]] = streamZipper.coflatten
    val gridZipper = GridZipper(largeStreamZipper)

    gridZipper.coflatMap(_.extract) shouldBe gridZipper
  }
  it should "have valid right identity with >1 elements" in pendingUntilFixed {
    val streamZipper = Zipper(LazyList(1, 2, 3, 4), 5, LazyList())
    val largeStreamZipper: Zipper[Zipper[Int]] = streamZipper.coflatten
    val gridZipper = GridZipper(largeStreamZipper)

    val result = gridZipper.coflatMap(_.extract)

    result shouldBe gridZipper
  }
  it should "have valid left identity" in {
    val streamZipper = Zipper(LazyList(), 2, LazyList())
    val largeStreamZipper: Zipper[Zipper[Int]] = streamZipper.coflatten
    val gridZipper = GridZipper(largeStreamZipper)

    gridZipper.coflatten.extract shouldBe gridZipper
  }
  it should "have valid left identity with >1 elements" in {
    val streamZipper = Zipper(LazyList(1, 2, 3, 4), 5, LazyList(6,7))
    val largeStreamZipper: Zipper[Zipper[Int]] = streamZipper.coflatten
    val gridZipper = GridZipper(largeStreamZipper)

    val result = gridZipper.coflatten.extract

    result shouldBe gridZipper
  }
  it should "have associativity" in {
    val streamZipper = Zipper(LazyList(), 2, LazyList())
    val largeStreamZipper: Zipper[Zipper[Int]] = streamZipper.coflatten
    val gridZipper = GridZipper(largeStreamZipper)

    gridZipper.coflatten.coflatten shouldBe gridZipper.coflatMap(_.coflatten)
  }
  it should "have associativity with > 1 elements" in pendingUntilFixed {
    val streamZipper = Zipper(LazyList(2,1), 3, LazyList(4,5))
    val largeStreamZipper: Zipper[Zipper[Int]] = streamZipper.coflatten
    val gridZipper = GridZipper(largeStreamZipper)

    gridZipper.coflatten.coflatten shouldBe gridZipper.coflatMap(_.coflatten)
  }
  it should "have valid coflatmap" in {
    val streamZipper = Zipper(LazyList(), 2, LazyList())
    val largeStreamZipper: Zipper[Zipper[Int]] = streamZipper.coflatten
    val gridZipper = GridZipper(largeStreamZipper)

    def f(gridZipper: GridZipper[Int]): Int = {
      gridZipper.extract + 1
    }

    gridZipper.coflatMap(f) shouldBe GridZipper(Zipper(LazyList(), 3, LazyList()).coflatten)
  }
  it should "handle more complicated coflatmap" in pendingUntilFixed {
    val streamZipper = Zipper(LazyList(1), 2, LazyList())
    val largeStreamZipper: Zipper[Zipper[Int]] = streamZipper.coflatten
    val gridZipper = GridZipper(largeStreamZipper)

    val expectedGridZipper: GridZipper[Int] = GridZipper(
      Zipper(
        LazyList(Zipper(LazyList(), 3, LazyList(4))),
        Zipper(LazyList(3), 4, LazyList()),
        LazyList()
      )
    )

    def f(gridZipper: GridZipper[Int]): Int = {
      gridZipper.extract + 2
    }

    gridZipper.coflatMap(f) shouldBe expectedGridZipper
  }
}
