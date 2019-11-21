package conway

import catlike.data.GridZipper
import catlike.syntax.gridZipper._
import catlike.syntax.streamZipper._

import scala.sys.process._

class Renderer(visualization: Visualization) {
  def clear: Int = "clear".!

  def cellRepresentation(value: Int): String = {
    val alive = visualization.alive
    val background = visualization.background
    if (value == 1) alive else background
  }

  def render(grid: GridZipper[Int]): String = {
    grid.map(i => cellRepresentation(i))
      .value
      .map(_.toList)
      .toList
      .map(_.mkString)
      .mkString("\n")
  }

  def frameRate(millis: Int): Unit = {
    Thread.sleep(millis)
  }

  def renderFrame(gridZipper: GridZipper[Int]): Unit = {
    clear
    println(render(gridZipper))
    frameRate(275)
  }
}
