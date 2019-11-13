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
  case class Turn(frame: Int, pinsDown: Stream[Int])

  def unfold[A, B](seed: A)(f: A => Option[(A, B)]): Stream[B] = {
    f(seed) match {
      case None => Stream.empty
      case Some((a, b)) => cons(b, unfold(a)(f))
    }
  }

  def roll(r: Random, upperBound: Int): Int = {
    // to express this as 10 pins elsewhere,
    // you'll need nextInt to be from 0 up to but excluding 11
    r.nextInt(upperBound + 1)
  }

  val isStrike: Int => Boolean = i => i == 10
  val isSpare: (Int, Int) => Boolean = (fst, snd) => fst + snd == 10

  def tenthFrame(first: Int, second: => Int, r: Random): Turn = {
    if (isStrike(first)) {
      val secondRoll = roll(r, 10)
      val thirdRoll = if (isStrike(secondRoll)) roll(r,10) else roll(r,10 - secondRoll)
      Turn(frame = 10, Stream(first, secondRoll, thirdRoll))
    }
    else if (isSpare(first, second)) {
      val third = roll(r,10)
      Turn(frame = 10, Stream(first, second, third))
    } else {
      Turn(frame = 10, Stream(first, second))
    }
  }

  def takeTurn(r: Random, frame: Int): Turn = {
    val first = roll(r,10)
    lazy val second = roll(r,10 - first)
    if (frame == 10) {
      tenthFrame(first, second, r)
    } else if (isStrike(first))
      Turn(frame, Stream(first))
    else
      Turn(frame, Stream(first, second))
  }

  val t = unfold(1)((frame: Int) => if (frame > 10) None else Some(frame + 1, takeTurn(Random, frame)))

  println(t.toList)

}

