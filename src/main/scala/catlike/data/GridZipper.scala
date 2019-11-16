package catlike.data

import catlike.Comonad

// 2 dimensions represented by nested StreamZippers
case class GridZipper[A](value: StreamZipper[StreamZipper[A]])

object GridZipper {
  import catlike.syntax.streamZipper._

  implicit def gridZipperComonad: Comonad[GridZipper] = {
    new Comonad[GridZipper] {
      override def extract[A](w: GridZipper[A]): A = w.value.focus.focus

      override def duplicate[A](w: GridZipper[A]): GridZipper[GridZipper[A]] = ???

      override def coflatMap[A, B](w: GridZipper[A])(f: GridZipper[A] => B): GridZipper[B] = map(duplicate(w))(f)

      override def map[A, B](fa: GridZipper[A])(f: A => B): GridZipper[B] = ???

      def nest[A](s: StreamZipper[StreamZipper[A]]): StreamZipper[StreamZipper[StreamZipper[A]]] = {
        val lefts: Stream[StreamZipper[StreamZipper[A]]] = Stream.iterate(s)(current => current.moveLeft)
          .tail
          .zip(s.left)
          .map(_._1)

        val rights: Stream[StreamZipper[StreamZipper[A]]] = Stream.iterate(s)(current => current.moveRight)
          .tail
          .zip(s.right)
          .map(_._1)

        StreamZipper(lefts, s, rights)
      }
    }
  }
}
