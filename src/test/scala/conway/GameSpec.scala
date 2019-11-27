package conway

import catlike.data.{GridZipper, Zipper}
import conway.Game.cellLifecycle
import org.scalatest._
import catlike.syntax.gridZipper._
import catlike.syntax.streamZipper._

class GameSpec extends FlatSpec with Matchers {
  val lrStream: Stream[Int] = Stream.fill(3)(0)
  val streamOfStreams: Zipper[Zipper[Int]] = Zipper(lrStream, 0, lrStream).duplicate
  val basicGrid = GridZipper(streamOfStreams)

  "cellLifecycle" should "keep board of all 0's the same" in {
    val result = basicGrid.coflatMap(x => cellLifecycle(x))
    println(result.prettyPrint)
    result.extract shouldBe 0
  }
  it should "set an arbitrary point" in {
    val setFocusB = basicGrid.setFocus(5)
    val randomN = setFocusB.north.setFocus(4)
    println(randomN.prettyPrint + "\n")
    println(randomN.west.setFocus(1).prettyPrint)
  }
  it should "set another arbitrary point" in {
    val setFocusB = basicGrid.setFocus(1)

    val south = setFocusB.south.setFocus(2)
    println(south.prettyPrint + "\n")

    val north = setFocusB.north.setFocus(3)
    println(north.prettyPrint + "\n")
  }
  it should "change live cell to dead when no surrounding cells are alive" in {
    val setCenter = basicGrid.setFocus(1)
    println(setCenter.prettyPrint + "\n")

    val result = setCenter.coflatMap(x => cellLifecycle(x))
    println(result.prettyPrint)
    result.extract shouldBe 0
  }
  it should "change dead cell to alive when surrounded by three live cells" in {
    val grid1: GridZipper[Int] = basicGrid.west.setFocus(1)

    println("set west")
    println(grid1.prettyPrint)

    val grid2 = grid1.north.setFocus(1)

    println("set north west")
    println(grid2.prettyPrint)

    val grid3 = grid2.east.setFocus(1)

    println("set north")
    println(grid3.prettyPrint)

    val centerGrid = grid3.south

    println("return center")
    println(centerGrid.prettyPrint)

    val result = centerGrid.coflatMap(cellLifecycle)
    println("result")
    println(result.prettyPrint)
    result.extract shouldBe 1

    println("south")
    println(result.south.prettyPrint)
    result.south.extract shouldBe 0
  }

}
