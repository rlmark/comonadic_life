package conway

import typeclasses.data.GridZipper
import cats.effect.{ExitCode, IO, IOApp, Sync, Timer}
import cats.syntax.all._
import conway.Game._
import fs2.Stream
import typeclasses.syntax.gridZipper._

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
    initialStateMap.getOrElse(coord, 0)
  }

  def gameLoop[F[_] : Timer : Sync]: F[Stream[F, GridZipper[Int]]] = {
    val console = new Console[F]()
    for {
      vis <- console.getVisualization
      width <- console.getWidth
      userShapeInput <- console.placeUserShapes
    } yield {
      val render = new Renderer[F](vis)
      Stream.iterate(
        buildGrid(coordinates => setCellValue(coordinates, userShapeInput), width) // build initial grid with user presents
      )(nextGeneration) // run subsequent generations over the seed grid
        .evalTap(grid => render.renderFrame(grid)) // prints image to console
        .zipLeft(Stream.awakeEvery[F](325.millis)) // sets the frame rate
    }
  }

  override def run(args: List[String]): IO[ExitCode] =
    gameLoop[IO].flatMap(_.compile.drain.map(_ => ExitCode.Success))
}
