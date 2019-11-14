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

  implicit class storeComonadSyntax[A](self: CartesianStore[A])(implicit c: Comonad[CartesianStore]) {
    def extract: A = c.extract(self)

    def duplicate: CartesianStore[CartesianStore[A]] = c.duplicate(self)

    def coflatMap[B](f: CartesianStore[A] => B): CartesianStore[B] = c.coflatMap(self)(f)

    def map[B](f: A => B): CartesianStore[B] = c.map(self)(f)
  }
}
