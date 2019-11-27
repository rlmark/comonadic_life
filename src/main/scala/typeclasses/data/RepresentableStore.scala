package typeclasses.data

import typeclasses.Representable

case class RepresentableStore[F[_], S, A](fa: F[A], index: S)(implicit R: Representable.Aux[F, S]) {
  def peek(s: S): A = R.index(fa)(s)

  def extract: A = peek(index)

  def coflatten: RepresentableStore[F, S, RepresentableStore[F, S, A]] = {
    val interimF: F[RepresentableStore[F, S, A]] = R.tabulate(i => RepresentableStore(fa, i))
    RepresentableStore(interimF, index)
  }
}
