import org.scalatest._

class StreamZipperSpec extends FlatSpec with Matchers {

  val initialZipper: StreamZipper[Int] = StreamZipper(Stream(4,3,2,1), 5, Stream(6,7,8))
  "StreamZipper" should "have valid focus"  in {
    initialZipper.focus shouldBe 5
  }
  it should "shift focus left" in {
    initialZipper.moveLeft shouldBe StreamZipper(Stream(3,2,1), 4, Stream(5,6,7,8))
  }
  it should "shift focus right" in {
    initialZipper.moveRight shouldBe StreamZipper(Stream(5,4,3,2,1), 6, Stream(7,8))
  }

}
