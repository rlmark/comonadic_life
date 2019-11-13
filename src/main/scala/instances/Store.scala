package instances

import catlike._

case class Store[S, A](query: S => A)(val index: S)

object StoreComonad {
  type Coordinates = (Int, Int)
  type StoreCoordinates[A] = Store[Coordinates, A]

  implicit def storeComonadInstance: Comonad[StoreCoordinates] = {
    new Comonad[StoreCoordinates] {
      override def counit[A](w: StoreCoordinates[A]): A = w.query(w.index)

      override def coflatten[A](w: StoreCoordinates[A]): StoreCoordinates[StoreCoordinates[A]] = Store(Store(w.query))(w.index)

      // don't think this is right but...
      override def coflatMap[A, B](w: StoreCoordinates[A])(f: StoreCoordinates[A] => B): StoreCoordinates[B] =
        Store((_: Coordinates) => f(w))(w.index)

      override def map[A, B](fa: StoreCoordinates[A])(f: A => B): StoreCoordinates[B] = {
        Store(fa.query.andThen(a => f(a)))(fa.index)
      }
    }
  }
}
