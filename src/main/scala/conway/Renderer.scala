package conway

import catlike.data.GridZipper
import catlike.syntax.gridZipper._
import catlike.syntax.streamZipper._
import scala.sys.process._
import scala.util.Random

object Renderer {
  def clear: Int = "clear".!

  def cellString(value: Int): String = {
    val alive = "\uD83E\uDD84"
    val rand = Random.nextInt(3)
    val background = if (rand == 0) "\uD83C\uDF32" else if (rand == 1) "\uD83C\uDF34" else "\uD83C\uDF33"
    if (value == 1) s"$alive" else s"$background"
  }

  def render(grid: GridZipper[Int]): String = {
    grid.map(i => cellString(i))
      .value
      .map(_.toList)
      .toList
      .map(_.mkString)
      .mkString("\n")
  }
}
