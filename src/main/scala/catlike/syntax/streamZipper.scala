package catlike.syntax

import catlike.Comonad

object streamZipper {
  import catlike.data.StreamZipper

  implicit class streamZipperSyntax[A](self: StreamZipper[A])(implicit c: Comonad[StreamZipper]) {
    def extract: A = c.extract(self)

    def duplicate: StreamZipper[StreamZipper[A]] = c.duplicate(self)

    def coflatMap[B](f: StreamZipper[A] => B): StreamZipper[B] = c.coflatMap(self)(f)

    def map[B](f: A => B): StreamZipper[B] = c.map(self)(f)
  }
}
