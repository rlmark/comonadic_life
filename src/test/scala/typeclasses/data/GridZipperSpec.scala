package typeclasses.data

import org.scalatest._
import typeclasses.data.Zipper._
import typeclasses.syntax.gridZipper._
import typeclasses.syntax.zipper._

class GridZipperSpec extends FlatSpec with Matchers {

  "extract" should "extract the focus" in {
    val streamZipper = Zipper(Stream(1), 2, Stream(3))
    val largeStreamZipper: Zipper[Zipper[Int]] = streamZipper.duplicate
    val gridZipper = GridZipper(largeStreamZipper)

    gridZipper.extract shouldBe 2
  }
  it should "shift left and focus" in {
    val streamZipper = Zipper(Stream(1), 2, Stream(3))
    val largeStreamZipper: Zipper[Zipper[Int]] = streamZipper.duplicate
    val gridZipper = GridZipper(largeStreamZipper)

    GridZipper(gridZipper.value.moveLeft).extract shouldBe 1
  }
  "duplicate" should "handle basic case" in {
    val streamZipper = Zipper(Stream(), 2, Stream())
    val largeStreamZipper: Zipper[Zipper[Int]] = streamZipper.duplicate
    val gridZipper = GridZipper(largeStreamZipper)

    val streamOfGrid: Zipper[Zipper[GridZipper[Int]]] = Zipper(
      Stream(),
      Zipper(
        Stream(),
        gridZipper,
        Stream()
      ),
      Stream()
    )
    val expectedGridZipper: GridZipper[GridZipper[Int]] = GridZipper(
      streamOfGrid
    )

    gridZipper.duplicate shouldBe expectedGridZipper
  }
  it should "duplicate more complicated zippers" in pendingUntilFixed {
    val streamZipper = Zipper(Stream(1), 2, Stream())
    val largeStreamZipper: Zipper[Zipper[Int]] = streamZipper.duplicate
    val gridZipper = GridZipper(largeStreamZipper)

    val expectedGridZipper: GridZipper[GridZipper[Int]] = GridZipper(
      Zipper(
        Stream(
          Zipper(
            Stream(),
            GridZipper(
              Zipper(
                Stream(),
                Zipper(Stream(), 1, Stream(2)),
                Stream(Zipper(Stream(1), 2, Stream()))
              )
            ),
            Stream(
              GridZipper(
                Zipper(
                  Stream(Zipper(Stream(), 1, Stream(2))),
                  Zipper(Stream(1), 2, Stream()),
                  Stream()
                )
              )
            )
          )
        ),
        Zipper(
          Stream(
            GridZipper(
              Zipper(
                Stream(),
                Zipper(Stream(), 1, Stream(2)),
                Stream(Zipper(Stream(1), 2, Stream()))
              )
            )
          ),
          GridZipper(
            Zipper(
              Stream(Zipper(Stream(), 1, Stream(2))),
              Zipper(Stream(1), 2, Stream()),
              Stream()
            )
          ),
          Stream()
        ),
        Stream()
      )
    )

    // GridZ[StreamZ[StreamZ[GridZ[StreamZ[StreamZ[Int]]]]]]]
    gridZipper.duplicate shouldBe expectedGridZipper
  }
  it should "have right identity" in {
    val streamZipper = Zipper(Stream(), 2, Stream())
    val largeStreamZipper: Zipper[Zipper[Int]] = streamZipper.duplicate
    val gridZipper = GridZipper(largeStreamZipper)

    gridZipper.coflatMap(_.extract) shouldBe gridZipper
  }
  it should "have valid right identity with >1 elements" in pendingUntilFixed {
    val streamZipper = Zipper(Stream(1, 2, 3, 4), 5, Stream())
    val largeStreamZipper: Zipper[Zipper[Int]] = streamZipper.duplicate
    val gridZipper = GridZipper(largeStreamZipper)

    val result = gridZipper.coflatMap(_.extract)

    result shouldBe gridZipper
  }
  it should "have valid left identity" in {
    val streamZipper = Zipper(Stream(), 2, Stream())
    val largeStreamZipper: Zipper[Zipper[Int]] = streamZipper.duplicate
    val gridZipper = GridZipper(largeStreamZipper)

    gridZipper.duplicate.extract shouldBe gridZipper
  }
  it should "have valid left identity with >1 elements" in {
    val streamZipper = Zipper(Stream(1, 2, 3, 4), 5, Stream(6,7))
    val largeStreamZipper: Zipper[Zipper[Int]] = streamZipper.duplicate
    val gridZipper = GridZipper(largeStreamZipper)

    val result = gridZipper.duplicate.extract

    result shouldBe gridZipper
  }
  it should "have associativity" in {
    val streamZipper = Zipper(Stream(), 2, Stream())
    val largeStreamZipper: Zipper[Zipper[Int]] = streamZipper.duplicate
    val gridZipper = GridZipper(largeStreamZipper)

    gridZipper.duplicate.duplicate shouldBe gridZipper.coflatMap(_.duplicate)
  }
  it should "have associativity with > 1 elements" in pendingUntilFixed {
    val streamZipper = Zipper(Stream(2,1), 3, Stream(4,5))
    val largeStreamZipper: Zipper[Zipper[Int]] = streamZipper.duplicate
    val gridZipper = GridZipper(largeStreamZipper)

    gridZipper.duplicate.duplicate shouldBe gridZipper.coflatMap(_.duplicate)
  }
  it should "have valid coflatmap" in {
    val streamZipper = Zipper(Stream(), 2, Stream())
    val largeStreamZipper: Zipper[Zipper[Int]] = streamZipper.duplicate
    val gridZipper = GridZipper(largeStreamZipper)

    def f(gridZipper: GridZipper[Int]): Int = {
      gridZipper.extract + 1
    }

    gridZipper.coflatMap(f) shouldBe GridZipper(Zipper(Stream(), 3, Stream()).duplicate)
  }
  it should "handle more complicated coflatmap" in pendingUntilFixed {
    val streamZipper = Zipper(Stream(1), 2, Stream())
    val largeStreamZipper: Zipper[Zipper[Int]] = streamZipper.duplicate
    val gridZipper = GridZipper(largeStreamZipper)

    val expectedGridZipper: GridZipper[Int] = GridZipper(
      Zipper(
        Stream(Zipper(Stream(), 3, Stream(4))),
        Zipper(Stream(3), 4, Stream()),
        Stream()
      )
    )

    def f(gridZipper: GridZipper[Int]): Int = {
      gridZipper.extract + 2
    }

    gridZipper.coflatMap(f) shouldBe expectedGridZipper
  }
}
