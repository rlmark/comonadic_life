package typeclasses

trait CoflatMap[F[_]] extends Functor[F] {

  // duplicates the context's structure - also called coflatten,
  // replaces every value in the data structure with its corresponding context
  //
  // Zipper
  // [1,(2),3] => duplicate =>
  // [
  //  [(1),2,3],
  //  [1,(2),3],
  //  [1,2,(3)]
  // ]

def duplicate[A](w: F[A]): F[F[A]]
  // given a value in a context and a function which transforms and extracts the value,
  // return the transformed value in the context
def coflatMap[A,B](w: F[A])(f: F[A] => B): F[B]  = {
  map(duplicate(w))(f)
}
}
