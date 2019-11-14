package bowling

object ScoreHelpers {
  val isStrike: Int => Boolean = i => i == 10
  val isSpare: (Int, Int) => Boolean = (fst, snd) => fst + snd == 10
}
