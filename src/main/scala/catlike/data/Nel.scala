package catlike.data

import scala.util.Try

case class Nel[A](head: A, tails: List[A]) {
  def at(i: Int): A = {
    if (i == 0) head else tails(i + 1)
  }
}

object Nel {
  def unsafeFromList[A](list: List[A]): Nel[A] = Nel(list.head, list.tail)
  def fromList[A](list:List[A]): Try[Nel[A]] = Try(list.head).map(Nel(_,list.tail))

  def apply[A](as: A*): Nel[A] = unsafeFromList(as.toList)
}
