package catlike.syntax

import catlike.Comonad

object nel {
  import catlike.data.Nel
  implicit class nelComonadSyntax[A](self: Nel[A])(implicit c: Comonad[Nel]) {
    def extract: A = c.extract(self)

    def duplicate: Nel[Nel[A]] = c.duplicate(self)

    def coflatMap[B](f: Nel[A] => B): Nel[B] = c.coflatMap(self)(f)

    def map[B](f: A => B): Nel[B] = c.map(self)(f)
  }
}
