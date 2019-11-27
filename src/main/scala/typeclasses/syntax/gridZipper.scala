package typeclasses.syntax

import typeclasses.Comonad
import typeclasses.data.GridZipper

object gridZipper {
  implicit class gridZipperSyntax[A](self: GridZipper[A])(implicit c: Comonad[GridZipper]) {
    def extract: A = c.extract(self)

    def duplicate: GridZipper[GridZipper[A]] = c.duplicate(self)

    def coflatMap[B](f: GridZipper[A] => B): GridZipper[B] = c.coflatMap(self)(f)

    def map[B](f: A => B): GridZipper[B] = c.map(self)(f)
  }
}
