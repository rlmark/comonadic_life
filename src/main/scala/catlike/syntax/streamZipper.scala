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

object storeComonad {
  import catlike.data.Store._

  implicit class storeComonadSyntax[A](self: CoordinateStore[A])(implicit c: Comonad[CoordinateStore]) {
    def extract: A = c.extract(self)

    def duplicate: CoordinateStore[CoordinateStore[A]] = c.duplicate(self)

    def coflatMap[B](f: CoordinateStore[A] => B): CoordinateStore[B] = c.coflatMap(self)(f)

    def map[B](f: A => B): CoordinateStore[B] = c.map(self)(f)
  }
}

object nelComonad {
  import catlike.data.Nel
  implicit class nelComonadSyntax[A](self: Nel[A])(implicit c: Comonad[Nel]) {
    def extract: A = c.extract(self)

    def duplicate: Nel[Nel[A]] = c.duplicate(self)

    def coflatMap[B](f: Nel[A] => B): Nel[B] = c.coflatMap(self)(f)

    def map[B](f: A => B): Nel[B] = c.map(self)(f)
  }
}
