package conway

import catlike.data.{GridZipper, StreamZipper}
import conway.Game.cellLifecycle
import org.scalatest._
import catlike.syntax.gridZipper._
import catlike.syntax.streamZipper._

class GameSpec extends FlatSpec with Matchers {

  "cellLifecycle" should "keep board of all 0's the same" in {
    val lrStream = Stream.fill(1)(0)
    val streamOfStreams = StreamZipper(lrStream, 0, lrStream).duplicate
    val basicGrid = GridZipper(streamOfStreams)

    val result = basicGrid.coflatMap(x => cellLifecycle(x))
    println(result.prettyPrint)
    result.extract shouldBe 0
  }
  it should "change alive cell to dead when no surrounding cells are alive" in {
    val lrStream = Stream.fill(1)(0)
    val streamOfStreams = StreamZipper(lrStream, 1, lrStream).duplicate
    val basicGrid = GridZipper(streamOfStreams)

    val result = basicGrid.coflatMap(x => cellLifecycle(x))
    println(result.prettyPrint)
    result.extract shouldBe 0
  }
  it should "change dead cell to alive when surrounded by three lives cells" in {
    val lrStream = Stream.fill(1)(0)
    val streamOfStreams = StreamZipper(lrStream, 0, lrStream).duplicate
    val grid1: GridZipper[Int] = GridZipper(streamOfStreams).west.setFocus(1)

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

    result.south.extract shouldBe 0
  }

}
