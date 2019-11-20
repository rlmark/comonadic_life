package conway

import catlike.data.GridZipper
import catlike.data.GridZipper._
import catlike.syntax.gridZipper._

object Game {

  /**
   * Any live cell with two or three neighbors survives.
   * Any dead cell with three live neighbors becomes a live cell.
   * All other live cells die in the next generation. Similarly, all other dead cells stay dead.
   **/

  def cellLifecycle(grid: GridZipper[Int]): Int = {
    val extracted = GridZipper.gridZipperComonad.extract(grid)
    val neighborList = GridZipper.getNeighbors(grid)

    (neighborList.sum, extracted) match {
      case (sum, 1) if sum == 2 || sum == 3 => 1
      case (3, 0) => 1
      case (_, 1) => 0
      case (_, x) => x
    }
  }

  def generation(grid: GridZipper[Int]): GridZipper[Int] = {
    grid.coflatMap(cellLifecycle)
  }
}
