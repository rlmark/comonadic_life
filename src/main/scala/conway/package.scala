
package object conway {

  type Coordinates = (Int, Int)

  implicit class InitOps(presetShapes: Map[Coordinates, Int]) {
    def at(coordinates: Coordinates): Map[Coordinates, Int] =
      presetShapes.map {
        case ((x, y), v) => ((x + coordinates._2, y + coordinates._1), v)
      }
  }

}
