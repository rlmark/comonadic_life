package catlike.instances

import catlike._

object StreamZipperComonad {
  implicit def zipperComonad: Comonad[StreamZipper] = new Comonad[StreamZipper] {
    override def extract[A](w: StreamZipper[A]): A = w.focus

    override def duplicate[A](w: StreamZipper[A]): StreamZipper[StreamZipper[A]] = {
      StreamZipper(w.streamLeftF(identity), w, w.streamRightF(identity))
    }

    override def coflatMap[A, B](w: StreamZipper[A])(f: StreamZipper[A] => B): StreamZipper[B] = {
      // Alternatively: StreamZipper(w.streamLeftF(f), f(w), w.streamRightF(f))
      map(duplicate(w))(f)
    }

    override def map[A, B](fa: StreamZipper[A])(f: A => B): StreamZipper[B] = {
      StreamZipper(fa.left.map(f) ,f(fa.focus), fa.right.map(f))
    }
  }
}
