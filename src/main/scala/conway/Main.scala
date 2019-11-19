package conway

import catlike.data.Store.Coordinates
import catlike.data.StreamZipper._
import catlike.data.{GridZipper, StreamZipper}
import catlike.syntax.streamZipper._
import catlike.syntax.gridZipper._
import Swarms._

import scala.util.Random

object Main extends App {

  def cellLifecycle(grid: GridZipper[Int]): Int = {
    val extracted = GridZipper.gridZipperComonad.extract(grid)
    val neighborList = GridZipper.getNeighbors(grid)

    (neighborList.sum, extracted) match {
      case (sum, 1) if sum == 2 || sum == 3 => 1
      case (3, 0) => 1
      case (_, 1) => 0
      case (_, x) => x
    }
  }

  def generation(grid: GridZipper[Int]): GridZipper[Int] = {
   grid.coflatMap(cellLifecycle)
  }

  def newGrid(values: List[List[Int]]): GridZipper[Int] = {
    GridZipper(fromList(values.map(fromList)))
  }

  def tabulate(fn: ((Int, Int)) => Int): GridZipper[Int] = {
    val extent = 20

    val coords: List[(Int, Int)] = (for {
      x <- 0 until extent
      y <- 0 until extent
    } yield (x, y)).toList

    val coordinates: List[List[(Int, Int)]] = coords.grouped(extent).toList

    newGrid(coordinates.map(_.map(e => fn(e))))
  }

  def fromList[A](items: List[A]): StreamZipper[A] = {
    val left = items.take(items.size)
    val right = items.drop(items.size)
    StreamZipper(left.tail.toStream, left.head, right.toStream)
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

  val initialState = (glider at(1, 1)) ++ (beacon at(16, 12)) ++ (blinker at(14, 5)) ++ (dieHard at (7,7))

  def setInitial(coord: (Int, Int)): Int =
    initialState.getOrElse(coord, 0)

  def gameLoop(): Unit = {
    val streamGrids: Stream[GridZipper[Int]] = Stream.iterate(tabulate(setInitial))(generation)
    streamGrids.foreach{f =>
      println(render(f))
      println("\u001b")
      Thread.sleep(300)
    }
  }

  gameLoop()
}
