object StreamZipperComonad {
  implicit def zipperComonad: Comonad[StreamZipper] = new Comonad[StreamZipper] {
    override def counit[A](w: StreamZipper[A]): A = w.focus
    override def cojoin[A](w: StreamZipper[A]): StreamZipper[StreamZipper[A]] = {
      StreamZipper(w.moveLeft.streamLeftF(identity), w, w.moveRight.streamRightF(identity))
    }
    override def coflatMap[A, B](w: StreamZipper[A])(f: StreamZipper[A] => B): StreamZipper[B] = {
      StreamZipper(w.moveLeft.streamLeftF(f), f(w), w.moveRight.streamRightF(f))
    }
  }
}
