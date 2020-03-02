package conway

import typeclasses.data.{Coordinates, Grid, GridZipper, Store}
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

  def format(store: Store[Coordinates, Int], width: Int, plane: List[Coordinates]): String = {
    val cells: List[String] = store
      .listExperiment{ _ => plane }
      .map(cellRepresentation)

    cells
      .grouped(width)
      .map(_.mkString)
      .mkString("\n")
  }

  def renderFrame(store: Store[Coordinates, Int], width: Int, plane: List[Coordinates]): F[Unit] = for {
    _ <- clear
    _ <- Sync[F].delay(println(format(store, width, plane)))
  } yield ()

  def renderFrame(gridZipper: GridZipper[Int]): F[Unit] = {
    for {
      _ <- clear
      _ <- Sync[F].delay(println(format(gridZipper)))
    } yield ()
  }
}
