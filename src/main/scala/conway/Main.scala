package conway

import catlike.data.GridZipper
import cats.effect.{ExitCode, IO, IOApp, Sync, Timer}
import conway.Game._
import conway.Swarms._
import fs2.{Stream => StreamF}
import scala.concurrent.duration._

object Main extends IOApp {

  type Coordinates = (Int, Int)

  def tabulate(fn: Coordinates => Int): GridZipper[Int] = {
    val width = 20

    val coords: List[Coordinates] = (for {
      x <- 0 until width
      y <- 0 until width
    } yield (x, y)).toList

    val coordinates: List[List[Coordinates]] = coords.grouped(width).toList
    val coordinatesToCellValues: List[List[Int]] = coordinates.map(_.map(e => fn(e)))

    GridZipper.fromLists(coordinatesToCellValues)
  }

  implicit class InitOps(pairs: Map[Coordinates, Int]) {
    def at(coordinates: Coordinates): Map[Coordinates, Int] = pairs.map {
      case ((x, y), v) => ((x + coordinates._2, y + coordinates._1), v)
    }
  }

  def setInitial(coord: (Int, Int)): Int = {
    val initialState: Map[(Int, Int), Int] = (glider at(1, 1))  ++ (beacon at(12, 16)) ++ (blinker at(5, 14)) ++ (dieHard at(7, 7))
    initialState.getOrElse(coord, 0)
  }

  def gameLoop[F[_]: Timer : Sync]: StreamF[F, GridZipper[Int]] = {
    val render = new Renderer[F](Visualization.Ocean) // pick a visualization
    StreamF.iterate(tabulate(setInitial))(generation) // run subsequent generations over a seed grid
      .evalTap(grid => render.renderFrame(grid))  // prints image to console
      .zipLeft(StreamF.awakeEvery[F](300.millis)) // sets the frame rate
  }

  override def run(args: List[String]): IO[ExitCode] =
    gameLoop[IO].compile.drain.map(_ => ExitCode.Success)
}
