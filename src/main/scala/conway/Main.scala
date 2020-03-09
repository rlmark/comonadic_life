package conway

import typeclasses.data.GridZipper
import typeclasses.syntax.gridZipper._
import cats.effect.{ExitCode, IO, IOApp, Sync, Timer}
import cats.syntax.all._
import conway.Game._
import fs2.Stream

import scala.concurrent.duration._

object Main extends IOApp {

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


  def setCellValue(coord: (Int, Int), initialStateMap: Cells): Int = {
    initialStateMap.getOrElse(coord, 0)
  }

  def gameLoop[F[_] : Timer : Sync]: F[Stream[F, GridZipper[Int]]] = {
    val console = new Console[F]()
    for {
      width <- console.getWidth
      board <- if(width == 47) random else inputs(console)
    } yield {
      val (vis, userShapeInput) = board
      val render = new Renderer[F](vis)
      Stream.iterate(
        buildGrid(coordinates => setCellValue(coordinates, userShapeInput), width) // build initial grid with user presents
      )(nextGeneration) // run subsequent generations over the seed grid
        .evalTap(grid => render.renderFrame(grid)) // prints image to console
        .zipLeft(Stream.awakeEvery[F](325.millis)) // sets the frame rate
    }
  }

  def inputs[F[_]: Sync](console: Console[F]): F[Board] =
    (console.getVisualization, console.placeUserShapes).mapN((v: Visualization, c: Cells) => (v,c))

  def random[F[_]: Sync]: F[Board] =
    Sync[F].delay((Visualization.Forest, (1 to 47).flatMap(i => Patterns(i % 5).shape.rnd).toMap))


  override def run(args: List[String]): IO[ExitCode] =
    gameLoop[IO].flatMap(_.compile.drain.map(_ => ExitCode.Success))
}
