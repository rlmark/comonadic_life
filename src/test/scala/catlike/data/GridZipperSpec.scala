package catlike.data

import catlike.data.Zipper._
import catlike.syntax.gridZipper._
import catlike.syntax.streamZipper._
import org.scalatest._

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
  it should "duplicate more complicated zippers" in {
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
  it should "have valid coflatmap" in {
    val streamZipper = Zipper(Stream(), 2, Stream())
    val largeStreamZipper: Zipper[Zipper[Int]] = streamZipper.duplicate
    val gridZipper = GridZipper(largeStreamZipper)

    def f(gridZipper: GridZipper[Int]): Int = {
      gridZipper.extract + 1
    }

    gridZipper.coflatMap(f) shouldBe GridZipper(Zipper(Stream(), 3, Stream()).duplicate)
  }
  it should "handle more complicated coflatmap" in {
    val streamZipper = Zipper(Stream(1), 2, Stream())
    val largeStreamZipper: Zipper[Zipper[Int]] = streamZipper.duplicate
    val gridZipper = GridZipper(largeStreamZipper)

    val expectedGridZipper: GridZipper[Int] = GridZipper(
      Zipper(
        Stream(Zipper(Stream(), 2, Stream(3))),
        Zipper(Stream(2), 3, Stream()),
        Stream()
      )
    )

    def f(gridZipper: GridZipper[Int]): Int = {
      gridZipper.extract + 1
    }

    gridZipper.coflatMap(f) shouldBe expectedGridZipper
  }
}
