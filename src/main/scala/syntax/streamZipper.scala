package syntax

import catlike.{Comonad, Functor}
import instances.StreamZipper

object streamZipper {
  implicit class streamZipperSyntax[A](self: StreamZipper[A])(implicit c: Comonad[StreamZipper]) {
    def counit: A = c.counit(self)
    def coflatten: StreamZipper[StreamZipper[A]] = c.coflatten(self)
    def coflatMap[B](f: StreamZipper[A] => B): StreamZipper[B]= c.coflatMap(self)(f)
  }
}
