package typeclasses.syntax

import typeclasses.CoflatMap
import typeclasses.data.Zipper

object zipper {

  implicit class zipperSyntax[A](self: Zipper[A])(implicit c: CoflatMap[Zipper]) {

    def duplicate: Zipper[Zipper[A]] = c.duplicate(self)

    def coflatMap[B](f: Zipper[A] => B): Zipper[B] = c.coflatMap(self)(f)

    def map[B](f: A => B): Zipper[B] = c.map(self)(f)
  }

}
