package conway

import catlike.data.{GridZipper, StreamZipper}
import GridZipper._
import catlike.syntax.gridZipper._
import catlike.syntax.streamZipper._

object Game extends App {

/**
    Any live cell with two or three neighbors survives.
    Any dead cell with three live neighbors becomes a live cell.
    All other live cells die in the next generation. Similarly, all other dead cells stay dead.
**/

  def cellLifecycle(grid: GridZipper[Int]): Int = {
    val neighborList = grid.getNeighbors
    (neighborList.sum, grid.extract) match {
      case (sum, 1) if sum == 2 || sum == 3 => 1
      case (3, 0) => 1
      case (_, 1) => 0
      case (_, x) => x
    }
  }

  val lrStream = Stream.fill(5)(0)
  val streamOfStreams = StreamZipper(lrStream, 0, lrStream).duplicate
  val basicGrid = GridZipper(streamOfStreams)

  println(basicGrid.coflatMap(x => cellLifecycle(x)).prettyPrint)

}
