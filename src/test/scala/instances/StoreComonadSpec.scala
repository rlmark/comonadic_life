package instances

import org.scalatest._

class StoreComonadSpec extends FlatSpec with Matchers {

  import StoreComonad._
  import syntax.storeComonad._

  val grid =

  "A StoreComonad" should "have a valid extract" in {
    val coordinates1: Coordinates = (0,0)
    val coordinates2: Coordinates = (1,0)

    def lookup(coordinates: Coordinates): Boolean = true
  }
}
