package conway

import cats.effect.{ExitCode, IO, IOApp, Sync, Timer}
import cats.syntax.all._
import fs2.Stream
import typeclasses.data.{Coordinates, Store}

import scala.concurrent.duration._

object StoreMain extends IOApp {

  def createCoordinateList(width: Int): List[Coordinates] = {
    (for {
      x <- 0 until width
      y <- 0 until width
    } yield Coordinates(x, y)).toList
  }

  def gameLoopS[F[_] : Timer : Sync]: F[Stream[F, Store[Coordinates, Int]]] = {
    val console = new Console[F]()
    for {
      vis <- console.getVisualization
      width <- console.getWidth
      userShapeInput <- console.placeUserShapes
      coordinateMap = userShapeInput.map { case ((x, y), v) => Coordinates(x, y) -> v }
      plane = createCoordinateList(width)
    } yield {
      val render = new Renderer[F](vis)
      Stream.iterate(
        Store((cs: Coordinates) => coordinateMap.getOrElse(cs, 0), Coordinates(0, 0)) // build initial grid with user presents
      )(StoreConway.nextGeneration) // run subsequent generations over the seed grid
        .evalTap(store => render.renderFrame(store, width, plane)) // prints image to console
        .zipLeft(Stream.awakeEvery[F](400.millis)) // sets the frame rate
    }
  }

  override def run(args: List[String]): IO[ExitCode] =
    gameLoopS[IO].flatMap(_.compile.drain.map(_ => ExitCode.Success))
}
