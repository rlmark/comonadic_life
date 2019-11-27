package typeclasses.syntax

import typeclasses.Comonad

object streamZipper {
  import typeclasses.data.Zipper

  implicit class streamZipperSyntax[A](self: Zipper[A])(implicit c: Comonad[Zipper]) {
    def extract: A = c.extract(self)

    def duplicate: Zipper[Zipper[A]] = c.duplicate(self)

    def coflatMap[B](f: Zipper[A] => B): Zipper[B] = c.coflatMap(self)(f)

    def map[B](f: A => B): Zipper[B] = c.map(self)(f)
  }
}
