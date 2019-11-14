package bowling

import instances.StreamZipper
import instances.StreamZipperComonad._
import syntax.streamZipper._
import ScoreHelpers._

object Game extends App {

  case class Frame(frame: Int, pinsDown: List[Int])

  val playerTurns: Stream[Frame] = PlayerSimulator.run

  val frameZipperToScore: StreamZipper[Frame] = StreamZipper(playerTurns.tail, playerTurns.head, Stream.empty)

  def calculateScore(frames: StreamZipper[Frame]): Int = {
    val Frame(frame, turnPoints) = frames.focus
    val currentFramePoints = turnPoints.sum
    if (frame == 10)
      currentFramePoints
    else if (isStrike(turnPoints.head)) {
      val leftStream: Stream[Int] = frames.coflatten.left.flatMap(s => s.focus.pinsDown.toStream)
      val next2: Stream[Int] = leftStream.take(2)
      currentFramePoints + next2.sum
    }
    else if (isSpare(turnPoints.head, turnPoints.tail.head)) {
      val leftFocus =  frames.moveLeft.focus
      currentFramePoints + leftFocus.pinsDown.head
    }
    else currentFramePoints
  }

  val scoreAll: StreamZipper[Int] = frameZipperToScore.coflatMap(calculateScore)
  val frameScores = scoreAll.focus +: scoreAll.left.toList

  println(s"The player's frames are: ${playerTurns.toList}")
  println(s"The scores for each frame are $frameScores")
  println(s"The total player score is ${frameScores.sum}")
}

