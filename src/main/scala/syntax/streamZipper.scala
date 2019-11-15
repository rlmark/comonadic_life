package syntax

import catlike.Comonad

object streamZipper {
  import instances.StreamZipper

  implicit class streamZipperSyntax[A](self: StreamZipper[A])(implicit c: Comonad[StreamZipper]) {
    def extract: A = c.extract(self)

    def duplicate: StreamZipper[StreamZipper[A]] = c.duplicate(self)

    def coflatMap[B](f: StreamZipper[A] => B): StreamZipper[B] = c.coflatMap(self)(f)

    def map[B](f: A => B): StreamZipper[B] = c.map(self)(f)
  }
}

object storeComonad {
  import instances.StoreComonad._

  implicit class storeComonadSyntax[A](self: CoordinateStore[A])(implicit c: Comonad[CoordinateStore]) {
    def extract: A = c.extract(self)

    def duplicate: CoordinateStore[CoordinateStore[A]] = c.duplicate(self)

    def coflatMap[B](f: CoordinateStore[A] => B): CoordinateStore[B] = c.coflatMap(self)(f)

    def map[B](f: A => B): CoordinateStore[B] = c.map(self)(f)
  }
}
