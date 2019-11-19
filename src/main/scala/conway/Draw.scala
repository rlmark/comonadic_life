package conway

import catlike.data.Store.Coordinates
import catlike.data.StreamZipper._
import catlike.data.{GridZipper, StreamZipper}
import catlike.syntax.streamZipper._
import catlike.syntax.gridZipper._

import scala.util.Random


object Draw extends App {

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

  def render(grid: GridZipper[Int]): String = {
    def cellString(value: Int): String = {
      val animal = "\uD83E\uDD84"
      val rand = Random.nextInt(3)
      val leaf = if (rand == 1) "\uD83C\uDF32" else if (rand == 2) "\uD83C\uDF34" else "\uD83C\uDF33"
      val test = if (value == 1) s" $animal " else s" $leaf "
      test
    }
    grid.map(i => cellString(i)).value.map(_.toList).toList.map(_.mkString).mkString("\n")
  }

  val glider = Map(
    (1, 0) -> 1,
    (2, 1) -> 1,
    (0, 2) -> 1,
    (1, 2) -> 1,
    (2, 2) -> 1,
  )

  val blinker = Map(
    (0, 0) -> 1,
    (1, 0) -> 1,
    (2, 0) -> 1
  )

  val beacon = Map(
    (0, 0) -> 1,
    (1, 0) -> 1,
    (0, 1) -> 1,
    (3, 2) -> 1,
    (2, 3) -> 1,
    (3, 3) -> 1
  )

  implicit class InitOps(pairs: Map[Coordinates, Int]) {
    def at(coord: Coordinates): Map[Coordinates, Int] = pairs.map {
      case ((x, y), v) => ((x + coord._1, y + coord._2), v)
    }
  }

  val initialState = (glider at(1, 1)) ++ (beacon at(16, 6)) ++ (blinker at(17, 5))

  def initialFn(coord: (Int, Int)): Int =
    initialState.getOrElse(coord, 0)

  def gameLoop(): Unit = {
    var current: GridZipper[Int] = tabulate(initialFn)
    while (true) {
      current = generation(current)
      val rendered = render(current)
      println(rendered)
      println("\u001b") // Clear term
      Thread.sleep(300)
    }
  }

  gameLoop()
}
