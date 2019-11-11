object syntax {
  implicit class streamZipperSyntax[A](self: StreamZipper[A])(implicit c: Comonad[StreamZipper]) {
    def coUnit: A = c.counit(self)
    def coflatten: StreamZipper[StreamZipper[A]] = c.coFlatten(self)
    def coflatMap[B](f: StreamZipper[A] => B): StreamZipper[B]= c.coflatMap(self)(f)
  }

}
