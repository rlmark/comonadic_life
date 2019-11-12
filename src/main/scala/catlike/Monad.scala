package catlike

trait Monad[M[_]] {
  def pure[A](a: A): M[A]
  def flatMap[A, B](m: M[A])(f: A => M[B]): M[B]
  def flatten[A](m: M[M[A]]): M[A]
}
