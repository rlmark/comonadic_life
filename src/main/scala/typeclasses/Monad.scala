package typeclasses

trait Monad[M[_]] extends Functor[M] {
  def pure[A](a: A): M[A]
  def flatten[A](m: M[M[A]]): M[A]
  def flatMap[A, B](m: M[A])(f: A => M[B]): M[B]
}
