package typeclasses.data

import typeclasses.CoflatMap
import typeclasses.data.Zipper._
import typeclasses.syntax.zipper._

// 2 dimensions represented by nested Zippers
case class GridZipper[A](value: Zipper[Zipper[A]]) {

  def maybeFocus: Option[A] = value.focus.flatMap(_.focus)

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

  def getNeighbors: List[Option[A]] = {
    List(
      this.north.value.focus.flatMap(_.focus),
      this.east.value.focus.flatMap(_.focus),
      this.south.value.focus.flatMap(_.focus),
      this.west.value.focus.flatMap(_.focus),
      this.north.east.value.focus.flatMap(_.focus),
      this.north.west.value.focus.flatMap(_.focus),
      this.south.east.value.focus.flatMap(_.focus),
      this.south.west.value.focus.flatMap(_.focus)
    )
  }
}

object GridZipper {

  def fromLists[A](lists: List[List[A]]): GridZipper[A] = {
    GridZipper(Zipper.fromList(lists.map(Zipper.fromList)))
  }

  implicit def gridZipperCoflatMap: CoflatMap[GridZipper] = {
    new CoflatMap[GridZipper] {

      override def duplicate[A](w: GridZipper[A]): GridZipper[GridZipper[A]] = {
        val s1: Zipper[Zipper[Zipper[A]]] = nest(w.value)
        val s2: Zipper[Zipper[Zipper[Zipper[A]]]] = nest(s1)
        val g1: GridZipper[Zipper[Zipper[A]]] = GridZipper(s2)
        val g2: GridZipper[GridZipper[A]] = map(g1)(GridZipper(_))
        g2
      }

      override def map[A, B](fa: GridZipper[A])(f: A => B): GridZipper[B] = GridZipper(fa.value.map(s => s.map(f)))

      private def nest[A](s: Zipper[Zipper[A]]): Zipper[Zipper[Zipper[A]]] = {
        val duplicateLefts: Stream[Zipper[Zipper[A]]] = {
//                    Zipper.unfold(s)(z => z.maybeLeft.flatMap(y =>  y.maybeLeft.map(x => (x,x))))
          Stream.iterate(s)(current => current.map(_.moveLeft))
            .tail
            .zip(s.left)
            .map(_._1)
        }

        val duplicateRights: Stream[Zipper[Zipper[A]]] =
//                  Zipper.unfold(s)(z => z.maybeRight.flatMap(y => y.maybeRight.map(x => (x,x))))
          Stream.iterate(s)(current => current.map(_.moveRight))
            .tail
            .zip(s.right)
            .map(_._1)

        Zipper(duplicateLefts.map(Option(_)), Option(s), duplicateRights.map(Option(_)))
      }
    }
  }
}
