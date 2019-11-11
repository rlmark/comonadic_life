case class Store[S, A](query: S => A)(val index: S)

object StoreComonad {
  type StoreCoordinates[A] = Store[(Int, Int), A]

  implicit def storeComonadInstance[S]: Comonad[StoreCoordinates] = {
    new Comonad[StoreCoordinates] {
      override def counit[A](w: StoreCoordinates[A]): A = w.query(w.index)

      override def coFlatten[A](w: StoreCoordinates[A]): StoreCoordinates[StoreCoordinates[A]] = ???

      override def coflatMap[A, B](w: StoreCoordinates[A])(f: StoreCoordinates[A] => B): StoreCoordinates[B] = ???
    }
  }
}
