import org.scalatest._

class StreamZipperComonadSpec extends FlatSpec with Matchers {
  import StreamZipperComonad._
  import syntax._

  val initialZipper: StreamZipper[Int] = StreamZipper(Stream(4,3,2,1), 5, Stream(6,7,8))

  "StreamZipperComonad" should "have a valid coUnit" in {
    initialZipper.coUnit shouldBe 5
  }
  it should "have a valid coJoin" in {
    val smallZipper: StreamZipper[Int] = StreamZipper(Stream(1), 2, Stream(3))

    val expected = StreamZipper(
      Stream(StreamZipper(Stream.empty, 1, Stream(2,3))),
      initialZipper,
      Stream(StreamZipper(Stream(2,1), 3, Stream.empty))
    )

    println(smallZipper.coJoin)

    // StreamZipper(Stream(StreamZipper(Stream(),1,Stream(2, ?)), ?),StreamZipper(Stream(4, ?),5,Stream(6, ?)),Stream(StreamZipper(Stream(2, ?),3,Stream()), ?))


    smallZipper.coJoin shouldBe expected
  }
}
