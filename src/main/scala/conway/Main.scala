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
  def buildGrid(setCellValue: Coordinates => Int, width: Int): GridZipper[Int] = {
    val coordinates: List[List[Coordinates]] = createCoordinateLists(width)
    val gridZipperCoordinates: GridZipper[(Int, Int)] = GridZipper.fromLists(coordinates)
    gridZipperCoordinates.map(setCellValue)
  }

  implicit class InitOps(presetShapes: Map[Coordinates, Int]) {
    def at(coordinates: Coordinates): Map[Coordinates, Int] = presetShapes.map {
      case ((x, y), v) => ((x + coordinates._2, y + coordinates._1), v)
    }
  }

  def setCellValue(coord: (Int, Int), initialStateMap: Map[Coordinates, Int]): Int = {
    val initialState: Map[(Int, Int), Int] = (glider.at(1, 1)) ++ (beacon at(12, 16)) ++ (blinker at(5, 14)) ++ (dieHard at(7, 7))
    initialStateMap.getOrElse(coord, 0) // Setting everything not in the shapes above to be 0
  }
  import cats.syntax.all._

  def gameLoop[F[_] : Timer : Sync]: F[StreamF[F, GridZipper[Int]]] = {
    val console = new Console[F]()
    // 1. Ask reader what shape they want to place, and where
    // 2. Build up a map based on that - sensitive to width of grid.
    for {
      vis <- console.getVisualization
      width <- console.getWidth
      buildShapes <- console.loop(Map.empty)
    } yield {
      val render = new Renderer[F](vis)
      StreamF.iterate(buildGrid(setCellValue(_ , buildShapes), width))(generation) // run subsequent generations over a seed grid
        .evalTap(grid => render.renderFrame(grid)) // prints image to console
        .zipLeft(StreamF.awakeEvery[F](300.millis)) // sets the frame rate
    }
//    val render = new Renderer[F](Visualization.Ocean) // pick a visualization
//
//    StreamF.iterate(buildGrid(setCellValue))(generation) // run subsequent generations over a seed grid
//      .evalTap(grid => render.renderFrame(grid)) // prints image to console
//      .zipLeft(StreamF.awakeEvery[F](300.millis)) // sets the frame rate
  }

  override def run(args: List[String]): IO[ExitCode] =
    gameLoop[IO].flatMap(_.compile.drain.map(_ => ExitCode.Success))
}
