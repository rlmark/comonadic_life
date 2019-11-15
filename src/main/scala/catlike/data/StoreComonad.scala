package catlike.data

import catlike.Comonad

object StoreComonad {
  // TODO: Make this a case class
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
}
