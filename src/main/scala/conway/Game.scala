package conway

import typeclasses.data.GridZipper
import typeclasses.data.GridZipper._
import typeclasses.syntax.gridZipper._

object Game {

  /**
   * Any live cell with two or three neighbors survives.
   * Any dead cell with three live neighbors becomes a live cell.
   * All other live cells die in the next generation. Similarly, all other dead cells stay dead.
   **/

  def cellLifecycle(grid: GridZipper[Int]): Int = {
    val neighborList: List[Int] = grid.getNeighbors
    (neighborList.sum, grid.extract) match {
      case (sum, 1) if sum == 2 || sum == 3 => 1
      case (3, 0) => 1
      case (_, 1) => 0
      case (_, x) => x
    }
  }

  def nextGeneration(grid: GridZipper[Int]): GridZipper[Int] = {
    grid.coflatMap(cellLifecycle)
  }
}
