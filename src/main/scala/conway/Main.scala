package conway

import catlike.data.GridZipper
import cats.effect.{ExitCode, IO, IOApp, Sync, Timer}
import conway.Game._
import conway.Swarms._
import fs2.{Stream => StreamF}
import catlike.syntax.gridZipper._

import scala.concurrent.duration._

object Main extends IOApp {

  type Coordinates = (Int, Int)

  def createCoordinateLists(width: Int): List[List[Coordinates]] = {

    val coords: List[Coordinates] = (for {
      x <- 0 until width
      y <- 0 until width
    } yield (x, y)).toList

    coords.grouped(width).toList
  }
  def buildGrid(setCellValue: Coordinates => Int): GridZipper[Int] = {
    val width = 20
    val coordinates: List[List[Coordinates]] = createCoordinateLists(width)
    val gridZipperCoordinates: GridZipper[(Int, Int)] = GridZipper.fromLists(coordinates)
    gridZipperCoordinates.map(setCellValue)
  }

  implicit class InitOps(presetShapes: Map[Coordinates, Int]) {
    def at(coordinates: Coordinates): Map[Coordinates, Int] = presetShapes.map {
      case ((x, y), v) => ((x + coordinates._2, y + coordinates._1), v)
    }
  }

  def setCellValue(coord: (Int, Int)): Int = {
    val initialState: Map[(Int, Int), Int] = (glider.at(1, 1)) ++ (beacon at(12, 16)) ++ (blinker at(5, 14)) ++ (dieHard at(7, 7))
    initialState.getOrElse(coord, 0) // Setting everything not in the shapes above to be 0
  }

  def gameLoop[F[_] : Timer : Sync]: StreamF[F, GridZipper[Int]] = {
    val render = new Renderer[F](Visualization.Ocean) // pick a visualization
    val initial: GridZipper[Int] = buildGrid { case (x, y) => setCellValue((x, y)) }
    import catlike.syntax.gridZipper._
    initial.coflatMap(generation)
    StreamF.iterate(initial)(generation) // run subsequent generations over a seed grid
      .evalTap(grid => render.renderFrame(grid)) // prints image to console
      .zipLeft(StreamF.awakeEvery[F](300000000.millis)) // sets the frame rate
  }

  override def run(args: List[String]): IO[ExitCode] =
    gameLoop[IO].compile.drain.map(_ => ExitCode.Success)
}
