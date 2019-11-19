package conway

import catlike.data.{GridZipper, StreamZipper}
import GridZipper._
import catlike.syntax.gridZipper._
import catlike.syntax.streamZipper._

import scala.util.Random

object Game extends App {

/**
    Any live cell with two or three neighbors survives.
    Any dead cell with three live neighbors becomes a live cell.
    All other live cells die in the next generation. Similarly, all other dead cells stay dead.
**/

  def cellLifecycle(grid: GridZipper[Int]): Int = {
    val neighborList = GridZipper.getNeighbors(grid)
    (neighborList.sum, grid.extract) match {
      case (sum, 1) if sum == 2 || sum == 3 => 1
      case (3, 0) => 1
      case (_, 1) => 0
      case (_, 0) => 0
    }
  }

  def makeEmptyGrid(width: Int): GridZipper[Int] = {
    val lrStream = Stream.fill(width / 2)(0)
    val streamOfStreams = StreamZipper(lrStream, 0, lrStream).duplicate
    GridZipper(streamOfStreams)
  }

  def generation(grid: GridZipper[Int]): GridZipper[Int] = {
    grid.coflatMap(x => cellLifecycle(x))
  }

  def cellString(value: Int): String = {
    val animal = "\uD83E\uDD84"
    val rand = Random.nextInt(3)
    val leaf = if (rand == 1) "\uD83C\uDF32" else if (rand == 2) "\uD83C\uDF34" else "\uD83C\uDF33"
    if (value == 1) s"$animal" else s"$leaf"
  }

  def render(grid: GridZipper[Int]): String = {
    grid.map(cellString).value.map(_.toList).toList.map(_.mkString).mkString("\n")
  }

  println(render(generation(makeEmptyGrid(20))))

}
