package catlike.data

import catlike.Comonad

case class Store[S, A](query: S => A, index: S)

object Store {
  type Coordinates = (Int, Int)
  type CoordinateStore[A] = Store[Coordinates, A]

  implicit def storeComonadInstance: Comonad[CoordinateStore] = {
    new Comonad[CoordinateStore] {
      override def extract[A](w: CoordinateStore[A]): A = w.query(w.index)

      override def duplicate[A](w: CoordinateStore[A]): CoordinateStore[CoordinateStore[A]] = Store(Store(w.query, _), w.index)

      override def coflatMap[A, B](w: CoordinateStore[A])(f: CoordinateStore[A] => B): CoordinateStore[B] =
        map(duplicate(w))(f)

      override def map[A, B](fa: CoordinateStore[A])(f: A => B): CoordinateStore[B] = {
        Store(fa.query.andThen(a => f(a)), fa.index)
      }
    }
  }

  type MatrixB = Matrix[Boolean]
  type GridCoordinateStore[A] = Store[MatrixB, A]

  def storeGrid(matrix: MatrixB): Store[Coordinates, MatrixB] = {
    Store[Coordinates, MatrixB](matrix.at, matrix.focus) // TODO
  }

  implicit def gridComonadInstance: Comonad[GridCoordinateStore] = {
    new Comonad[GridCoordinateStore] {
      override def extract[A](w: GridCoordinateStore[A]): A = ???

      override def duplicate[A](w: GridCoordinateStore[A]): GridCoordinateStore[GridCoordinateStore[A]] = ???

      override def coflatMap[A, B](w: GridCoordinateStore[A])(f: GridCoordinateStore[A] => B): GridCoordinateStore[B] = ???

      override def map[A, B](fa: GridCoordinateStore[A])(f: A => B): GridCoordinateStore[B] = ???
    }
  }
}
