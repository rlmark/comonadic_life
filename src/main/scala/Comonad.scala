trait Comonad[W[_]] {
  // exctracts a value from the context
  def counit[A](w: W[A]): A
  // duplicating the effect's structure
  def coflatten[A](w: W[A]): W[W[A]]

  def coflatMap[A,B](w: W[A])(f: W[A] => B): W[B]
}

