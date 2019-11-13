package bowling

import catlike.Monoid
import instances.StreamZipper

import scala.collection.immutable.Stream.cons
import scala.util.Random

class Player(val name: String) {
  // This should be a Zipper(Index,PinCount)
  def takeTurn(frameIndex: Int, r: Random = Random): List[Int] = {
    val first = r.nextInt(11) // Upper bound is exclusive
    val second = r.nextInt(11 - first)
    if (frameIndex == 10) {
      if (first == 10) {
        val localSecond = r.nextInt(11)
        val localThird = if (localSecond == 10) r.nextInt(11) else r.nextInt(11 - localSecond)
        List(first, localSecond, localThird)
      }
      else if (first + second == 10) {
        List(first, second, r.nextInt(11))
      } else {
        List(first, second)
      }
    } else if (first == 10)
      List(first)
    else
      List(first, second)
  }


}

object testTurns extends App {
  case class Turn(frame: Int, pinsDown: List[Int])

  def roll(r: Random): Int = {
    r.nextInt(11)
  }

  def unfold[A, B](seed: A)(f: A => Option[(A, B)]): Stream[B] = {
    f(seed) match {
      case None => Stream.empty
      case Some((a: A, b: B)) => cons(b, unfold(a)(f))
    }
  }

  def tenthFrame(first: Int, second: Int, r: Random): Turn = {
    if (first == 10) {
      val secondRoll = r.nextInt(11)
      val thirdRoll = if (secondRoll == 10) r.nextInt(11) else r.nextInt(11 - secondRoll)
      Turn(frame = 10, List(first, secondRoll, thirdRoll))
    }
    else if (first + second == 10) {
      Turn(frame = 10, List(first, second, r.nextInt(11)))
    } else {
      Turn(frame = 10, List(first, second))
    }
  }

  def takeTurn(r: Random, frame: Int): Turn = {
    val first = r.nextInt(11) // Upper bound is exclusive
    lazy val second = r.nextInt(11 - first)
    if (frame == 10) {
      tenthFrame(first, second, r)
    } else if (first == 10)
      Turn(frame, List(first))
    else
      Turn(frame, List(first, second))
  }

  val t = unfold(1)((frame: Int) => if (frame > 10) None else Some(frame + 1, takeTurn(Random, frame)))

  println(t.toList)

}

