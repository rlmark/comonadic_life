package conway

import catlike.data.GridZipper
import catlike.data.Store.Coordinates

import conway.Game._
import conway.Renderer._
import conway.Swarms._

object Main extends App {

  def tabulate(fn: (Coordinates) => Int): GridZipper[Int] = {
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
    def at(coord: Coordinates): Map[Coordinates, Int] = pairs.map {
      case ((x, y), v) => ((x + coord._1, y + coord._2), v)
    }
  }

  def setInitial(coord: (Int, Int)): Int = {
    val initialState: Map[(Int, Int), Int] = (glider at(1, 1)) ++ (beacon at(16, 12)) ++ (blinker at(14, 5)) ++ (dieHard at(7, 7))
    initialState.getOrElse(coord, 0)
  }

  def gameLoop(): Unit = {
    val streamGrids: Stream[GridZipper[Int]] = Stream.iterate(tabulate(setInitial))(generation)
    streamGrids.foreach { f =>
      clear
      println(render(f))
      frameRate(275)
    }
  }

  gameLoop()
}
