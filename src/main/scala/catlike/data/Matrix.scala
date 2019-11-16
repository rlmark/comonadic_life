package catlike.data

import catlike.Comonad
import catlike.data.Store.Coordinates

case class Matrix[A](value: Nel[Nel[A]], focus: Coordinates) {
  def at(coordinates: Coordinates): A = value.at(coordinates._2).at(coordinates._1)
}

object Matrix {
  implicit def gridComonadInstance: Comonad[Matrix] = {
    new Comonad[Matrix] {
      override def extract[A](w: Matrix[A]): A = ???

      override def duplicate[A](w: Matrix[A]): Matrix[Matrix[A]] = ???

      override def coflatMap[A, B](w: Matrix[A])(f: Matrix[A] => B): Matrix[B] = ???

      override def map[A, B](fa: Matrix[A])(f: A => B): Matrix[B] = ???
    }
  }
}
