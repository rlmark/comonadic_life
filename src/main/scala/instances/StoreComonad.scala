package instances

import catlike.Comonad

object StoreComonad {
  // TODO: Make this a case class
  type Coordinates = (Int, Int)
  type CartesianStore[A] = Store[Coordinates, A]

  implicit def storeComonadInstance: Comonad[CartesianStore] = {
    new Comonad[CartesianStore] {
      override def extract[A](w: CartesianStore[A]): A = w.query(w.index)

      override def duplicate[A](w: CartesianStore[A]): CartesianStore[CartesianStore[A]] = Store(Store(w.query))(w.index)

      override def coflatMap[A, B](w: CartesianStore[A])(f: CartesianStore[A] => B): CartesianStore[B] =
        map(duplicate(w))(f)

      override def map[A, B](fa: CartesianStore[A])(f: A => B): CartesianStore[B] = {
        Store(fa.query.andThen(a => f(a)))(fa.index)
      }
    }
  }
}
