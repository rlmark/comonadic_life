package conway

import catlike.data.GridZipper
import catlike.syntax.gridZipper._
import catlike.syntax.streamZipper._
import conway.Visualization.Ocean

import scala.sys.process._

object Renderer {
  def clear: Int = "clear".!

  def cellString(value: Int): String = {
    val alive = Ocean.alive
    val background = Ocean.background
    if (value == 1) alive else background
  }

  def typeset(grid: GridZipper[Int]): String = {
    grid.map(i => cellString(i))
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
    println(typeset(gridZipper))
    frameRate(275)
  }
}
