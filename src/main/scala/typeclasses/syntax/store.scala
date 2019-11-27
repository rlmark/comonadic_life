package typeclasses.syntax

import typeclasses.Comonad

object store {
  import typeclasses.data.Store._

  implicit class storeComonadSyntax[A](self: CoordinateStore[A])(implicit c: Comonad[CoordinateStore]) {
    def extract: A = c.extract(self)

    def duplicate: CoordinateStore[CoordinateStore[A]] = c.duplicate(self)

    def coflatMap[B](f: CoordinateStore[A] => B): CoordinateStore[B] = c.coflatMap(self)(f)

    def map[B](f: A => B): CoordinateStore[B] = c.map(self)(f)
  }
}
