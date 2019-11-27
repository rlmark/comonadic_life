package typeclasses.syntax

import typeclasses.Comonad
import typeclasses.data.Zipper

object zipper {
  implicit class zipperSyntax[A](self: Zipper[A])(implicit c: Comonad[Zipper]) {
    def extract: A = c.extract(self)

    def duplicate: Zipper[Zipper[A]] = c.duplicate(self)

    def coflatMap[B](f: Zipper[A] => B): Zipper[B] = c.coflatMap(self)(f)

    def map[B](f: A => B): Zipper[B] = c.map(self)(f)
  }
}
