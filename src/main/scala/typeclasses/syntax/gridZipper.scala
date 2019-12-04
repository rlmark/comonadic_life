package typeclasses.syntax

import typeclasses.CoflatMap
import typeclasses.data.GridZipper

object gridZipper {

  implicit class gridZipperSyntax[A](self: GridZipper[A])(implicit c: CoflatMap[GridZipper]) {

    def duplicate: GridZipper[GridZipper[A]] = c.duplicate(self)

    def coflatMap[B](f: GridZipper[A] => B): GridZipper[B] = c.coflatMap(self)(f)

    def map[B](f: A => B): GridZipper[B] = c.map(self)(f)
  }

}
