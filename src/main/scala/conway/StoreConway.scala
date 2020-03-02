package conway

import typeclasses.data.{Coordinates, Store}
import typeclasses.Comonad

object StoreConway {
    /**
     * Any live cell with two or three neighbors survives.
     * Any dead cell with three live neighbors becomes a live cell.
     * All other live cells die in the next generation. Similarly, all other dead cells stay dead.
     **/

    def cellLifecycle(store: Store[Coordinates,Int]): Int = {
      val neighboringValues: List[Int] = store.listExperiment(index => index.neighboringCoords)
      (neighboringValues.sum, store.extract) match {
        case (sum, 1) if sum == 2 || sum == 3 => 1
        case (3, 0) => 1
        case (_, 1) => 0
        case (_, x) => x
      }
    }

    def nextGeneration(store: Store[Coordinates,Int]): Store[Coordinates,Int] = {
      implicitly[Comonad[Store[Coordinates, *]]].coflatMap(store)(cellLifecycle)
    }
}
