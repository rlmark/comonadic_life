package conway.data

import cats.{Comonad, Eq}
import cats.syntax.comonad._
import cats.syntax.functor._

// 2 dimensions represented by nested Zippers
case class GridZipper[A](value: Zipper[Zipper[A]]) {

  def setFocus(a: A): GridZipper[A] = {
    val inner: Zipper[A] = value.focus.setFocus(a)
    GridZipper(value.setFocus(inner))
  }

  def prettyPrint: String =
    value.toList.map(x => x.prettyPrint).mkString("\n")

  def north: GridZipper[A] =
    GridZipper(value.moveLeft)

  def south: GridZipper[A] =
    GridZipper(value.moveRight)

  def east: GridZipper[A] =
    GridZipper(value.map(xAxis => xAxis.moveRight))

  def west: GridZipper[A] =
    GridZipper(value.map(xAxis => xAxis.moveLeft))

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

  def fromLists[A](lists: List[List[A]]): GridZipper[A] =
    GridZipper(Zipper.fromList(lists.map(Zipper.fromList)))

  implicit def gridZipperEq[A: Eq]: Eq[GridZipper[A]] =
    Eq.by(_.value)

  implicit val gridZipperComonad: Comonad[GridZipper] =
    new Comonad[GridZipper] {
      override def extract[A](w: GridZipper[A]): A =
        w.value.focus.focus

      override def coflatten[A](w: GridZipper[A]): GridZipper[GridZipper[A]] = {
        val s1: Zipper[Zipper[Zipper[A]]] = nest(w.value)
        val s2: Zipper[Zipper[Zipper[Zipper[A]]]] = nest(s1)
        val g1: GridZipper[Zipper[Zipper[A]]] = GridZipper(s2)
        val g2: GridZipper[GridZipper[A]] = map(g1)(GridZipper(_))
        g2
      }

      override def coflatMap[A, B](w: GridZipper[A])(f: GridZipper[A] => B): GridZipper[B] =
        map(coflatten(w))(f)

      override def map[A, B](gz: GridZipper[A])(f: A => B): GridZipper[B] =
        GridZipper(gz.value.map(z => z map f))

      private def nest[A](s: Zipper[Zipper[A]]): Zipper[Zipper[Zipper[A]]] = {
        val duplicateLefts: LazyList[Zipper[Zipper[A]]] = {
          //          Zipper.unfold(s)(z => z.maybeLeft.flatMap(y =>  y.maybeLeft.map(x => (x,x))))
          LazyList.iterate(s)(current => current.map(_.moveLeft))
            .tail
            .zip(s.left)
            .map(_._1)
        }

        val duplicateRights: LazyList[Zipper[Zipper[A]]] =
        //          Zipper.unfold(s)(z => z.maybeRight.flatMap(y => y.maybeRight.map(x => (x,x))))
          LazyList.iterate(s)(current => current.map(_.moveRight))
            .tail
            .zip(s.right)
            .map(_._1)

        Zipper(duplicateLefts, s, duplicateRights)
      }
    }
}
