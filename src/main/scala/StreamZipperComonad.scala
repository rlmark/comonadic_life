object StreamZipperComonad {
  implicit def zipperComonad: Comonad[StreamZipper] = new Comonad[StreamZipper] {
    override def counit[A](w: StreamZipper[A]): A = w.focus
    override def coflatten[A](w: StreamZipper[A]): StreamZipper[StreamZipper[A]] = {
      StreamZipper(w.streamLeftF(identity), w, w.streamRightF(identity))
    }
    override def coflatMap[A, B](w: StreamZipper[A])(f: StreamZipper[A] => B): StreamZipper[B] = {
      StreamZipper(w.streamLeftF(f), f(w), w.streamRightF(f))
    }
  }
}
