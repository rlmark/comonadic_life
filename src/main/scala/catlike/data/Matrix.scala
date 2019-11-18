package catlike.data

import catlike.Comonad
import catlike.data.Store.Coordinates

case class Matrix[A](value: Nel[Nel[A]]) {
  def at(coordinates: Coordinates): A = value.at(coordinates._2).at(coordinates._1)
}

object Matrix {
  def nest[A](v: Matrix[A]): Matrix[Matrix[A]] = {
    // Think about this, do we want the outer matrix's focus to be
    // Matrix(v.value, v.focus)
    ???
  }

  implicit def gridComonadInstance: Comonad[Matrix] = {
    new Comonad[Matrix] {
      override def extract[A](w: Matrix[A]): A = w.value.head.head

      override def duplicate[A](w: Matrix[A]): Matrix[Matrix[A]] = ???

      override def coflatMap[A, B](w: Matrix[A])(f: Matrix[A] => B): Matrix[B] = ???

      override def map[A, B](fa: Matrix[A])(f: A => B): Matrix[B] = ???
    }
  }
}
