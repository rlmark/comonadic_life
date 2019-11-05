trait Comonad[W[_]] {
  def counit[A](w: W[A]): A
  def coflatMap[A,B](w: W[A])(f: A => W[B]): W[W[B]]
  def cojoin[A](w: W[A]): W[W[A]]
}
