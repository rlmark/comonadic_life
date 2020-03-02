package conway

import typeclasses.data.Grid
import typeclasses.data.Grid._
import typeclasses.syntax.grid._

object GridConway {

  /**
   * Any live cell with two or three neighbors survives.
   * Any dead cell with three live neighbors becomes a live cell.
   * All other live cells die in the next generation. Similarly, all other dead cells stay dead.
   **/

  def cellLifecycle(grid: Grid[Int]): Int = {
    val neighborList: List[Int] = grid.getNeighborhood
    (neighborList.sum, grid.extract) match {
      case (sum, 1) if sum == 2 || sum == 3 => 1
      case (3, 0) => 1
      case (_, 1) => 0
      case (_, x) => x
    }
  }

  def nextGeneration(grid: Grid[Int]): Grid[Int] = {
    grid.coflatMap(cellLifecycle)
  }
}
