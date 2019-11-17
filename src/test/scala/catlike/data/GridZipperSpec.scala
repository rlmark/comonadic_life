package catlike.data

import catlike.data.StreamZipper._
import catlike.syntax.gridZipper._
import catlike.syntax.streamZipper._
import org.scalatest._

class GridZipperSpec extends FlatSpec with Matchers {

  "extract" should "extract the focus" in {
    val streamZipper = StreamZipper(Stream(1), 2, Stream(3))
    val largeStreamZipper: StreamZipper[StreamZipper[Int]] = streamZipper.duplicate
    val gridZipper = GridZipper(largeStreamZipper)

    gridZipper.extract shouldBe 2
  }
  it should "shift left and focus" in {
    val streamZipper = StreamZipper(Stream(1), 2, Stream(3))
    val largeStreamZipper: StreamZipper[StreamZipper[Int]] = streamZipper.duplicate
    val gridZipper = GridZipper(largeStreamZipper)

    GridZipper(gridZipper.value.moveLeft).extract shouldBe 1
  }
  "duplicate" should "handle basic case" in {
    val streamZipper = StreamZipper(Stream(), 2, Stream())
    val largeStreamZipper: StreamZipper[StreamZipper[Int]] = streamZipper.duplicate
    val gridZipper = GridZipper(largeStreamZipper)

    val streamOfGrid: StreamZipper[StreamZipper[GridZipper[Int]]] = StreamZipper(
      Stream(),
      StreamZipper(
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
    val streamZipper = StreamZipper(Stream(1), 2, Stream())
    val largeStreamZipper: StreamZipper[StreamZipper[Int]] = streamZipper.duplicate
    val gridZipper = GridZipper(largeStreamZipper)

    val expectedGridZipper: GridZipper[GridZipper[Int]] = GridZipper(
      StreamZipper(
        Stream(
          StreamZipper(
            Stream(),
            GridZipper(
              StreamZipper(
                Stream(),
                StreamZipper(Stream(), 1, Stream(2)),
                Stream(StreamZipper(Stream(1), 2, Stream()))
              )
            ),
            Stream(
              GridZipper(
                StreamZipper(
                  Stream(StreamZipper(Stream(), 1, Stream(2))),
                  StreamZipper(Stream(1), 2, Stream()),
                  Stream()
                )
              )
            )
          )
        ),
        StreamZipper(
          Stream(
            GridZipper(
              StreamZipper(
                Stream(),
                StreamZipper(Stream(), 1, Stream(2)),
                Stream(StreamZipper(Stream(1), 2, Stream()))
              )
            )
          ),
          GridZipper(
            StreamZipper(
              Stream(StreamZipper(Stream(), 1, Stream(2))),
              StreamZipper(Stream(1), 2, Stream()),
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
    val streamZipper = StreamZipper(Stream(), 2, Stream())
    val largeStreamZipper: StreamZipper[StreamZipper[Int]] = streamZipper.duplicate
    val gridZipper = GridZipper(largeStreamZipper)

    def f(gridZipper: GridZipper[Int]): Int = {
      gridZipper.extract + 1
    }

    gridZipper.coflatMap(f) shouldBe GridZipper(StreamZipper(Stream(), 3, Stream()).duplicate)
  }
  it should "handle more complicated coflatmap" in {
    val streamZipper = StreamZipper(Stream(1), 2, Stream())
    val largeStreamZipper: StreamZipper[StreamZipper[Int]] = streamZipper.duplicate
    val gridZipper = GridZipper(largeStreamZipper)

    val expectedGridZipper: GridZipper[Int] = GridZipper(
      StreamZipper(
        Stream(StreamZipper(Stream(), 2, Stream(3))),
        StreamZipper(Stream(2), 3, Stream()),
        Stream()
      )
    )

    def f(gridZipper: GridZipper[Int]): Int = {
      gridZipper.extract + 1
    }

    gridZipper.coflatMap(f) shouldBe expectedGridZipper
  }
}
