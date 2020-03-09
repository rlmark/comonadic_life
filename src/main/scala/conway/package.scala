import scala.util.Random

package object conway {

  type Coordinates = (Int, Int)
  type Cells = Map[Coordinates, Int]
  type Board = (Visualization, Cells)

  implicit class InitOps(presetShapes: Cells) {
    def at(coordinates: Coordinates): Cells = presetShapes.map {
      case ((x, y), v) => ((x + coordinates._2, y + coordinates._1), v)
    }

    def rnd: Cells =
      at((Random.nextInt(48), Random.nextInt(48)))

  }
}
