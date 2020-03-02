import conway.Main.Coordinates

package object conway {
  implicit class InitOps(presetShapes: Map[Coordinates, Int]) {
    def at(coordinates: Coordinates): Map[Coordinates, Int] = presetShapes.map {
      case ((x, y), v) => ((x + coordinates._2, y + coordinates._1), v)
    }
  }
  implicit class InitCoordinatesOps(presetShapes: Map[typeclasses.data.Coordinates, Int]) {
    def at(coordinates: typeclasses.data.Coordinates): Map[typeclasses.data.Coordinates, Int] = presetShapes.map {
      case (typeclasses.data.Coordinates(x, y), v) => (typeclasses.data.Coordinates(x + coordinates.x, y + coordinates.y), v)
    }
  }
}
