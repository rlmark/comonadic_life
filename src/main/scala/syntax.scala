object syntax {

  implicit class streamZipperSyntax[A](self: StreamZipper[A])(implicit c: Comonad[StreamZipper]) {
    def coUnit: A = c.counit(self)
    def coJoin: StreamZipper[StreamZipper[A]] = c.cojoin(self)
    def coflatMap[B](f: StreamZipper[A] => B): StreamZipper[B]= c.coflatMap(self)(f)
  }
}
