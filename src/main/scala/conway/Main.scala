package conway

import catlike.data.Store.Coordinates
import catlike.data.StreamZipper._
import catlike.data.{GridZipper, StreamZipper}
import catlike.syntax.streamZipper._
import catlike.syntax.gridZipper._
import Swarms._
import Game._

import scala.util.Random

object Main extends App {

  def newGrid(values: List[List[Int]]): GridZipper[Int] = {
    GridZipper(fromList(values.map(fromList)))
  }

  def tabulate(fn: (Coordinates) => Int): GridZipper[Int] = {
    val width = 20

    val coords: List[Coordinates] = (for {
      x <- 0 until width
      y <- 0 until width
    } yield (x, y)).toList

    val coordinates: List[List[Coordinates]] = coords.grouped(width).toList

    newGrid(coordinates.map(_.map(e => fn(e))))
  }

  def cellString(value: Int): String = {
    val alive = "\uD83E\uDD84"
    val rand = Random.nextInt(3)
    val background = if (rand == 1) "\uD83C\uDF32" else if (rand == 2) "\uD83C\uDF34" else "\uD83C\uDF33"
    if (value == 1) s"$alive" else s"$background"
  }

  def render(grid: GridZipper[Int]): String = {
    grid.map(i => cellString(i)).value.map(_.toList).toList.map(_.mkString).mkString("\n")
  }

  implicit class InitOps(pairs: Map[Coordinates, Int]) {
    def at(coord: Coordinates): Map[Coordinates, Int] = pairs.map {
      case ((x, y), v) => ((x + coord._1, y + coord._2), v)
    }
  }

  def setInitial(coord: (Int, Int)): Int = {
    val initialState = (glider at(1, 1)) ++ (beacon at(16, 12)) ++ (blinker at(14, 5)) ++ (dieHard at (7,7))
    initialState.getOrElse(coord, 0)
  }

  def gameLoop(): Unit = {
    val streamGrids: Stream[GridZipper[Int]] = Stream.iterate(tabulate(setInitial))(generation)
    streamGrids.foreach{f =>
      println(render(f))
      println("\u001b")
      Thread.sleep(275)
    }
  }

  gameLoop()
}
