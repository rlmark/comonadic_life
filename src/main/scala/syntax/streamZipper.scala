package syntax

import catlike.Comonad

object streamZipper {
  import instances.StreamZipper

  implicit class streamZipperSyntax[A](self: StreamZipper[A])(implicit c: Comonad[StreamZipper]) {
    def counit: A = c.counit(self)

    def coflatten: StreamZipper[StreamZipper[A]] = c.coflatten(self)

    def coflatMap[B](f: StreamZipper[A] => B): StreamZipper[B] = c.coflatMap(self)(f)

    def map[B](f: A => B): StreamZipper[B] = c.map(self)(f)
  }
}

object storeComonad {
  import instances.StoreComonad._

  implicit class storeComonadSyntax[A](self: StoreCoordinates[A])(implicit c: Comonad[StoreCoordinates]) {
    def counit: A = c.counit(self)

    def coflatten: StoreCoordinates[StoreCoordinates[A]] = c.coflatten(self)

    def coflatMap[B](f: StoreCoordinates[A] => B): StoreCoordinates[B] = c.coflatMap(self)(f)

    def map[B](f: A => B): StoreCoordinates[B] = c.map(self)(f)
  }
}
