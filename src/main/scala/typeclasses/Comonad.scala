package typeclasses

trait Comonad[W[_]] extends CoflatMap[W] {
  // extracts a value from the context - also sometimes called counit
  def extract[A](w: W[A]): A
}
