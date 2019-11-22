package catlike.data

import catlike.Functor
import catlike.data.Store.Coordinates

case class Matrix[A](value: Nel[Nel[A]], focus: Coordinates) {
  // Technically unsafe, maybe integrate modulo logic here.
  def at(coordinates: Coordinates): A = value.at(coordinates._2).at(coordinates._1)
}

object Matrix {
  import Nel._
  import catlike.syntax.nel._

  implicit def matrixFunctorInstance: Functor[Matrix] = {
    new Functor[Matrix] {
      override def map[A, B](fa: Matrix[A])(f: A => B): Matrix[B] = Matrix(fa.value.map(_.map(f)), fa.focus)
    }
  }
}
