package catlike.data

import catlike.Comonad

case class Nel[A](head: A, tail: List[A]) {
  def at(i: Int): A = {
    if (i == 0) head else tail(i + 1)
  }

  def toList: List[A] = head :: tail

  def coflatMap[B](f: Nel[A] => B): Nel[B] = {
    @scala.annotation.tailrec
    def loop(as: List[A], acc: List[B]): List[B] =
      as match {
        case Nil => acc
        case a :: as =>
          val b =  f(Nel(a, as))
          loop(as, b :: acc)
      }
    Nel(f(this), loop(tail, List.empty[B]))
  }

  def length: Int = 1 + tail.length
}

object Nel {
  def unsafeFromList[A](list: List[A]): Nel[A] = Nel(list.head, list.tail)

  def fromList[A](list:List[A]): Option[Nel[A]] = list.headOption.map(Nel(_,list.tail))

  def of[A](head: A, as: A*): Nel[A] = Nel(head, as.toList)

  implicit def nelComonad: Comonad[Nel] = {
    new Comonad[Nel] {
      override def extract[A](w: Nel[A]): A = w.head

      override def duplicate[A](w: Nel[A]): Nel[Nel[A]] = w.coflatMap(identity[Nel[A]])

      override def map[A, B](fa: Nel[A])(f: A => B): Nel[B] = {
        Nel(f(fa.head), fa.tail.map(f))
      }
    }
  }
}
