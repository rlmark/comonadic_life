package typeclasses.data

import typeclasses.Comonad

// Think about defining a fallback typeclass for A
case class Grid[A](value: Stream[Stream[A]], coords: Coordinates) {
  // Note, unsafe - incorporate modulo logic
  def focus: A = {
    value(coords.y)(coords.x)
  }

  // Sets the value at the coordinates and returns a Grid focused on those coordinates
  def set(newVal: A, newCoords: Coordinates): Grid[A] = {
    val currentGrid = this.value
    val newGrid: Stream[Stream[A]] = currentGrid.updated(
      newCoords.y,
      currentGrid(newCoords.y).updated(newCoords.x, newVal)
    )
    Grid(newGrid, newCoords)
  }

  def getNeighborhood: List[A] = {
    coords.neighboringCoords.map(c => value(c.y)(c.x))
  }

}

case class Coordinates(x: Int, y: Int) {
  def neighboringCoords: List[Coordinates] = {
    List(
      // north
      Coordinates(x, y + 1),
      // south
      Coordinates(x, y - 1),
      // east
      Coordinates(x + 1, y),
      // west
      Coordinates(x - 1, y),
      // northeast
      Coordinates(x + 1, y + 1),
      // southeast
      Coordinates(x + 1, y - 1),
      // northwest
      Coordinates(x - 1, y + 1),
      // southwest
      Coordinates(x - 1, y - 1)
    )
  }
}

object Grid {
  implicit def gridComonad: Comonad[Grid] = new Comonad[Grid] {
    override def extract[A](w: Grid[A]): A = w.focus

    override def duplicate[A](w: Grid[A]): Grid[Grid[A]] = {
      val grid: Stream[Stream[Grid[A]]] = w.value.zipWithIndex.map{ case(streamA, y) =>
        streamA.zipWithIndex.map{ case(a, x) =>
          Grid(w.value, Coordinates(x,y))
        }
      }
      Grid(grid, w.coords)
    }

    override def map[A, B](fa: Grid[A])(f: A => B): Grid[B] = {
      val streamB: Stream[Stream[B]] =
        fa.value.map(
          rows => rows.map(
            columns => f(columns)
          )
        )
      Grid(streamB, fa.coords)
    }
  }
}
