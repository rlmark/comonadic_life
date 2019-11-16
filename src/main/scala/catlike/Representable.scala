package catlike

trait Representable[F[_]] {
  type Representation

  def F: Functor[F]

  def tabulate[X](f: Representation => X): F[X]

  def index[X]: F[X] => Representation => X
}

object Representable {
  // Helps set the Representation type of our Representable Functor to be what we specify as X.
  type Aux[F[_], X] = Representable[F] {type Representation = X}

  def apply[F[_]](implicit ev: Representable[F]): Representable.Aux[F, ev.Representation] = ev
}
