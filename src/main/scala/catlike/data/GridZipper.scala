package catlike.data

import catlike.{Comonad, Monoid}
import catlike.data.StreamZipper._
import catlike.syntax.gridZipper._
import catlike.syntax.streamZipper._

// 2 dimensions represented by nested StreamZippers
case class GridZipper[A](value: StreamZipper[StreamZipper[A]]) {

  def setFocus(a: A): GridZipper[A] = {
    val inner: StreamZipper[A] = value.focus.setFocus(a)
    GridZipper(value.setFocus(inner))
  }

  def prettyPrint: String = {
    value.toList.map(x => x.prettyPrint).mkString("\n")
  }

  def north: GridZipper[A] = {
    GridZipper(value.moveLeft)
  }

  def south: GridZipper[A] = {
    GridZipper(value.moveRight)
  }

  def east: GridZipper[A] = {
    GridZipper(value.map(xAxis => xAxis.moveRight))
  }

  def west: GridZipper[A] = {
    GridZipper(value.map(xAxis => xAxis.moveLeft))
  }

  def getNeighbors: List[A] = {
    List(
      this.north.extract,
      this.east.extract,
      this.south.extract,
      this.west.extract,
      this.north.east.extract,
      this.north.west.extract,
      this.south.east.extract,
      this.south.west.extract
    )
  }
}

object GridZipper {

  def fromLists[A](lists: List[List[A]]): GridZipper[A] = {
    GridZipper(StreamZipper.fromList(lists.map(StreamZipper.fromList)))
  }

  implicit def gridZipperComonad: Comonad[GridZipper] = {
    new Comonad[GridZipper] {
      override def extract[A](w: GridZipper[A]): A = w.value.focus.focus

      override def duplicate[A](w: GridZipper[A]): GridZipper[GridZipper[A]] = {
        val s1: StreamZipper[StreamZipper[StreamZipper[A]]] = nest(w.value)
        val s2: StreamZipper[StreamZipper[StreamZipper[StreamZipper[A]]]] = nest(s1)
        val g1: GridZipper[StreamZipper[StreamZipper[A]]] = GridZipper(s2)
        val g2: GridZipper[GridZipper[A]] = map(g1)(GridZipper(_))
        g2
      }

      override def coflatMap[A, B](w: GridZipper[A])(f: GridZipper[A] => B): GridZipper[B] = map(duplicate(w))(f)

      override def map[A, B](fa: GridZipper[A])(f: A => B): GridZipper[B] = GridZipper(fa.value.map(s => s.map(f)))

      private def nest[A](s: StreamZipper[StreamZipper[A]]): StreamZipper[StreamZipper[StreamZipper[A]]] = {
        val duplicateLefts: Stream[StreamZipper[StreamZipper[A]]] =
          Stream.iterate(s)(current => current.map(_.moveLeft))
          .tail
          .zip(s.left)
          .map(_._1)

        val duplicateRights: Stream[StreamZipper[StreamZipper[A]]] =
          Stream.iterate(s)(current => current.map(_.moveRight))
          .tail
          .zip(s.right)
          .map(_._1)

        StreamZipper(duplicateLefts, s, duplicateRights)
      }
    }
  }
}
