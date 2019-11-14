package bowling

import bowling.Game.Frame

import scala.collection.immutable.Stream.cons
import scala.util.Random
import ScoreHelpers._

object PlayerSimulator {

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

  def tenthFrame(first: Int, second: => Int, r: Random): Frame = {
    if (isStrike(first)) {
      val secondRoll = roll(r, 10)
      val thirdRoll = if (isStrike(secondRoll)) roll(r,10) else roll(r,10 - secondRoll)
      Frame(frame = 10, List(first, secondRoll, thirdRoll))
    }
    else if (isSpare(first, second)) {
      val third = roll(r,10)
      Frame(frame = 10, List(first, second, third))
    } else {
      Frame(frame = 10, List(first, second))
    }
  }

  def takeTurn(r: Random, frame: Int): Frame = {
    val first = roll(r,10)
    lazy val second = roll(r,10 - first)
    if (frame == 10) {
      tenthFrame(first, second, r)
    } else if (isStrike(first))
      Frame(frame, List(first))
    else
      Frame(frame, List(first, second))
  }

  def run: Stream[Frame] = unfold(1)((frame: Int) => if (frame > 10) None else Some(frame + 1, takeTurn(Random, frame)))
}
