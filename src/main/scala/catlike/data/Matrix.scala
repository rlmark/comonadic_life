package catlike.data

import catlike.Comonad
import catlike.data.Store.Coordinates

case class Matrix[A](value: Nel[Nel[A]], focus: Coordinates) {
  // Technically unsafe, maybe integrate modulo logic here.
  def at(coordinates: Coordinates): A = value.at(coordinates._2).at(coordinates._1)
}

object Matrix {
  import Nel._
  import catlike.syntax.nel._

  implicit def gridComonadInstance: Comonad[Matrix] = {
    new Comonad[Matrix] {
      override def extract[A](w: Matrix[A]): A = w.at(w.focus)

      override def duplicate[A](w: Matrix[A]): Matrix[Matrix[A]] = {
        val maxY: Int = w.value.length
        val maxX: Int = w.value.head.length

        val nestedNel: Nel[Nel[Matrix[A]]] = ???

        Matrix(nestedNel, w.focus)
      }

      override def map[A, B](fa: Matrix[A])(f: A => B): Matrix[B] = Matrix(fa.value.map(_.map(f)), fa.focus)
    }
  }
}
