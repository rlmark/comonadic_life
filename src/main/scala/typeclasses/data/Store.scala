package typeclasses.data

import typeclasses.{Comonad, Functor}

import scala.collection.mutable

case class Store[S, A](query: S => A, index: S) {
  // Apply the query function to the current index
  def extract: A = query(index)

  // Create a Store where the value is another Store
  def duplicate: Store[S, Store[S, A]] = {
    Store(s => Store(query, s), index)
  }

  // apply the function f to the value
  def map[B](f: A => B): Store[S, B] =
    Store(Store.memoize(query.andThen(f)), index)

  // At any position S, regardless of current index,
  // apply the query function to get the value at that index, A
  def peek(s: S): A = query(s)

  // A tangible version of the experiment function.
  def listExperiment(fn: S => List[S]): List[A] = {
    fn(index).map(query)
  }

  // A special function on store.
  // Because it's kinda annoying to have to do a bunch of peek's in a series.
  // If I have a function that operates on the index S to produce a functor of S,
  // give me a functor of A
  def experiment[F[_] : Functor](fn: S => F[S]): F[A] = {
    implicitly[Functor[F]].map(fn(index))(query)
  }

  // Hmmmm which is better?
  def duplicate2: Store[S, Store[S, A]] = {
    this match {
      case Store(f, s) => Store(Store(f, _), s)
    }
  }

  def duplicate3: Store[S, Store[S, A]] = {
    val Store(q, i) = this
    Store(Store(q, _), i)
  }
}

object Store {
  // If you don't memoize the store, sadness ensues
  def memoize[I, O](f: I => O): I => O = new mutable.HashMap[I, O]() {
    override def apply(key: I): O = getOrElseUpdate(key, f(key))
  }

  implicit def storeComonad[S]: Comonad[Store[S, *]] = new Comonad[Store[S, *]] {
    override def extract[A](w: Store[S, A]): A = w.extract

    override def duplicate[A](w: Store[S, A]): Store[S, Store[S, A]] = w.duplicate

    override def map[A, B](fa: Store[S, A])(f: A => B): Store[S, B] = fa.map(f)
  }
}
