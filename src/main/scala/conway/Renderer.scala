package conway

import typeclasses.data.GridZipper
import typeclasses.syntax.gridZipper._
import typeclasses.syntax.zipper._
import cats.effect.Sync
import cats.syntax.all._

import scala.sys.process._

class Renderer[F[_] : Sync](visualization: Visualization) {

  def clear: F[Int] = Sync[F].delay("clear".!)

  def cellRepresentation(value: Int): String = {
    val alive = visualization.alive
    val background = visualization.background
    if (value == 1) alive else background
  }

  def format(grid: GridZipper[Int]): String = {
    grid.map(i => cellRepresentation(i))
      .value
      .map(_.toList)
      .toList
      .map(_.mkString)
      .mkString("\n")
  }

  def renderFrame(gridZipper: GridZipper[Int]): F[Unit] = {
    for {
      _ <- clear
      _ <- Sync[F].delay(println(format(gridZipper)))
    } yield ()
  }
}
