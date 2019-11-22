package catlike.data

import catlike.Comonad

case class Store[S, A](query: S => A, index: S) {
  def extract: A = query(index)
  def extend[B](f: Store[S,A] => B): Store[S, B] =
    Store(s => f(Store(query, s)), index)
  def duplicate: Store[S, Store[S,A]] =
    extend(identity)
  def map[B](f: A => B): Store[S,B] =
    extend(s => f(s.extract))
  def seek(s: S): Store[S, A] = duplicate.query(s)
}

object Store {
  type Coordinates = (Int, Int)
  type CoordinateStore[A] = Store[Coordinates, A]

  implicit def storeComonadInstance: Comonad[CoordinateStore] = {
    new Comonad[CoordinateStore] {
      override def extract[A](w: CoordinateStore[A]): A = w.query(w.index)

      override def duplicate[A](w: CoordinateStore[A]): CoordinateStore[CoordinateStore[A]] = Store(Store(w.query, _), w.index)

      override def map[A, B](fa: CoordinateStore[A])(f: A => B): CoordinateStore[B] = {
        Store(fa.query.andThen(a => f(a)), fa.index)
      }
    }
  }

  type IntMatrix = Matrix[Int]
  type GridCoordinateStore[A] = Store[IntMatrix, A]
  implicit def gridComonadInstance: Comonad[GridCoordinateStore] = {
    new Comonad[GridCoordinateStore] {
      override def extract[A](w: GridCoordinateStore[A]): A = w.query(w.index)

      override def duplicate[A](w: GridCoordinateStore[A]): GridCoordinateStore[GridCoordinateStore[A]] = Store(Store(w.query, _), w.index)

      override def map[A, B](fa: GridCoordinateStore[A])(f: A => B): GridCoordinateStore[B] = Store(fa.query.andThen(a => f(a)), fa.index)
    }
  }
}
