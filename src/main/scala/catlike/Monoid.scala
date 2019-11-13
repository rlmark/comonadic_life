package catlike

trait Monoid[A] {
  def empty: A
  def combine(a1: A, a2: A ): A
}
