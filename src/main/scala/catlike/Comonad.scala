package catlike

// Comonads have a Functor instance
trait Comonad[W[_]] extends Functor[W] {
  // extracts a value from the context - also sometimes called counit
  def extract[A](w: W[A]): A
  // duplicates the context's structure - also called coflatten
  def duplicate[A](w: W[A]): W[W[A]]
  // given an value in a context and a function which transforms and extracts the value,
  // return the transformed value in the context
  def coflatMap[A,B](w: W[A])(f: W[A] => B): W[B]
}
