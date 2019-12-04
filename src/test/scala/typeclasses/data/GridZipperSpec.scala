package typeclasses.data

import org.scalatest._
import typeclasses.data.Zipper._
import typeclasses.syntax.gridZipper._
import typeclasses.syntax.zipper._

class GridZipperSpec extends FlatSpec with Matchers {

  "extract" should "extract the focus" in {
    val streamZipper = Zipper(Stream(Option(1)), Option(2), Stream(Option(3)))
    val largeStreamZipper: Zipper[Zipper[Int]] = streamZipper.duplicate
    val gridZipper = GridZipper(largeStreamZipper)

    gridZipper.maybeFocus.get shouldBe 2
  }
  it should "shift left and focus" in {
    val streamZipper = Zipper(Stream(Option(1)), Option(2), Stream(Option(3)))
    val largeStreamZipper: Zipper[Zipper[Int]] = streamZipper.duplicate
    val gridZipper = GridZipper(largeStreamZipper)

    GridZipper(gridZipper.value.moveLeft).maybeFocus.get shouldBe 1
  }
  "duplicate" should "handle basic case" in {
    val streamZipper = Zipper(Stream(), Option(2), Stream())
    val largeStreamZipper: Zipper[Zipper[Int]] = streamZipper.duplicate
    val gridZipper = GridZipper(largeStreamZipper)

    val streamOfGrid: Zipper[Zipper[GridZipper[Int]]] = Zipper(
      Stream(),
      Option(
        Zipper(
          Stream(),
          Option(gridZipper),
          Stream()
        )
      ),
      Stream()
    )
    val expectedGridZipper: GridZipper[GridZipper[Int]] = GridZipper(
      streamOfGrid
    )

    gridZipper.duplicate shouldBe expectedGridZipper
  }
  it should "duplicate more complicated zippers" in pendingUntilFixed {
    val streamZipper = Zipper(Stream(Option(1)), Option(2), Stream())
    val largeStreamZipper: Zipper[Zipper[Int]] = streamZipper.duplicate
    val gridZipper = GridZipper(largeStreamZipper)

    val expectedGridZipper: GridZipper[GridZipper[Int]] = GridZipper(
      Zipper(
        Stream(
          Some(
            Zipper(
              Stream(),
              Some(
                GridZipper(
                  Zipper(
                    Stream(),
                    Some(Zipper(Stream(), None, Stream(Some(1), Some(2)))),
                    Stream(Some(Zipper(Stream(Some(1)), Some(2), Stream())))
                  )
                )
              ),
              Stream(
                Some(
                  GridZipper(
                    Zipper(
                      Stream(Some(Zipper(Stream(), Some(1), Stream(Some(2))))),
                      Some(Zipper(Stream(Some(1)), Some(2), Stream())),
                      Stream()
                    )
                  )
                )
              )
            )
          )
        ),
        Some(expectedFocus),
        Stream()
      )
    )

    // GridZ[StreamZ[StreamZ[GridZ[StreamZ[StreamZ[Int]]]]]]]
    gridZipper.duplicate shouldBe expectedGridZipper
  }
  it should "have associativity" in {
    val streamZipper = Zipper(Stream(), Some(2), Stream())
    val largeStreamZipper: Zipper[Zipper[Int]] = streamZipper.duplicate
    val gridZipper = GridZipper(largeStreamZipper)

    gridZipper.duplicate.duplicate shouldBe gridZipper.coflatMap(_.duplicate)
  }
  it should "have associativity with > 1 elements" in pendingUntilFixed {
    val streamZipper = Zipper(Stream(1).map(Option(_)), Some(2), Stream())
    val largeStreamZipper: Zipper[Zipper[Int]] = streamZipper.duplicate
    val gridZipper = GridZipper(largeStreamZipper)

    pprint.pprintln(gridZipper.duplicate.duplicate)

    println()

    pprint.pprintln(gridZipper.coflatMap(_.duplicate))

    gridZipper.duplicate.duplicate shouldBe gridZipper.coflatMap(_.duplicate)
  }
  it should "have valid coflatmap" in {
    val streamZipper = Zipper(Stream(), Some(2), Stream())
    val largeStreamZipper: Zipper[Zipper[Int]] = streamZipper.duplicate
    val gridZipper = GridZipper(largeStreamZipper)

    def f(gridZipper: GridZipper[Int]): Int = {
      gridZipper.maybeFocus.getOrElse(0) + 1
    }

    gridZipper.coflatMap(f) shouldBe GridZipper(Zipper(Stream(), Some(3), Stream()).duplicate)
  }
  it should "handle more complicated coflatmap" in pendingUntilFixed {
    val streamZipper = Zipper(Stream(Some(1)), Some(2), Stream())
    val largeStreamZipper: Zipper[Zipper[Int]] = streamZipper.duplicate
    val gridZipper = GridZipper(largeStreamZipper)

    val expectedGridZipper: GridZipper[Int] = GridZipper(
      Zipper(
        Stream(Some(Zipper(Stream(), Some(3), Stream(Some(4))))),
        Some(Zipper(Stream(Some(3)), Some(4), Stream())),
        Stream()
      )
    )

    def f(gridZipper: GridZipper[Int]): Int = {
      gridZipper.maybeFocus.getOrElse(0) + 2
    }

    gridZipper.coflatMap(f) shouldBe expectedGridZipper
  }
}
