package typeclasses.syntax

import typeclasses.Comonad
import typeclasses.data.Grid

object grid {
  implicit class gridSyntax[A](self: Grid[A])(implicit c: Comonad[Grid]){
    def extract: A = c.extract(self)

    def duplicate: Grid[Grid[A]] = c.duplicate(self)

    def coflatMap[B](f: Grid[A] => B): Grid[B] = c.coflatMap(self)(f)

    def map[B](f: A => B): Grid[B] = c.map(self)(f)
  }
}
