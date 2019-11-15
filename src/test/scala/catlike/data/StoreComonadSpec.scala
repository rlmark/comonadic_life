package catlike.data

import org.scalatest._

class StoreComonadSpec extends FlatSpec with Matchers {

  import StoreComonad._
  import catlike.syntax.storeComonad._

  def isOrigin(coordinates: Coordinates): Boolean = if (coordinates == (0,0)) true else false
  val origin: Coordinates = (0,0)
  val otherCoordinates: Coordinates = (1,0)

  "A StoreComonad" should "have a valid extract" in {
    val originStore: CoordinateStore[Boolean] = Store(isOrigin, origin)
    originStore.extract shouldBe true

    val nonOriginStore: CoordinateStore[Boolean] = Store(isOrigin, otherCoordinates)
    nonOriginStore.extract shouldBe false
  }

  it should "have a valid duplicate" in {
    val nonOriginStore: CoordinateStore[Boolean] = Store(isOrigin, otherCoordinates)
    // Note: Fix[F[_]] might come in handy if nesting these end up getting more complicated
    val expected: CoordinateStore[CoordinateStore[Boolean]] = Store(Store(isOrigin, _), otherCoordinates)
    nonOriginStore.duplicate shouldBe expected
  }

  it should "have a valid coflatMap" in {
    val nonOriginStore: CoordinateStore[Boolean] = Store(isOrigin, otherCoordinates)
    def nextToOrigin(currentStore: CoordinateStore[Boolean]): Boolean = {
      val (x,y) = currentStore.index
      val xNearOrigin = x + 1 == 0 || x - 1 == 0 || x == 0
      val yNearOrigin = y + 1 == 0 || y - 1 == 0 || y == 0
      xNearOrigin && yNearOrigin
    }
    nonOriginStore.coflatMap(nextToOrigin).extract shouldBe true
  }
}
