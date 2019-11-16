package bowling

import catlike.data.StreamZipper
import catlike.syntax.streamZipper._
import ScoreHelpers._

object Game extends App {

  case class Frame(frame: Int, pinsDown: List[Int])

  val playerTurns: Stream[Frame] = PlayerSimulator.run

  // NOTE: this is probably a sign that StreamZipper is an overpowered data structure for what I'm doing
  // TODO: It's likely that the comonad I need is the one for NonEmptyList.
  val frameZipperToScore: StreamZipper[Frame] = StreamZipper(playerTurns.tail, playerTurns.head, Stream.empty)

  def calculateScore(frames: StreamZipper[Frame]): Int = {
    val Frame(frame, turnPoints) = frames.focus
    val currentFramePoints = turnPoints.sum
    if (frame == 10)
      currentFramePoints
    else if (isStrike(turnPoints.head)) {
      // Take the current frame and duplicate its structure, creating a StreamZipper of StreamZippers where every frame is in focus.
      // Taking the left value of that structure gives us a stream of "future" StreamZippers excluding the current frame in question,
      // flatMapping over the stream to get the focus' pin count stream gives us a continuous stream of values to the left of us.
      // In this way, we don't care about iterating through subsequent frames for our Strike calculation.
      val leftStream: Stream[Int] = frames
        .duplicate
        .left
        .flatMap((s: StreamZipper[Frame]) =>
          s.focus.pinsDown.toStream
        )
      val next2: Stream[Int] = leftStream.take(2)
      currentFramePoints + next2.sum
    }
    else if (isSpare(turnPoints.head, turnPoints.tail.head)) {
      val leftFocus =  frames.moveLeft.focus
      currentFramePoints + leftFocus.pinsDown.head
    }
    else currentFramePoints
  }

  //
  val scoreAll: StreamZipper[Int] = frameZipperToScore.coflatMap(calculateScore)
  val frameScores = scoreAll.focus +: scoreAll.left.toList

  println(s"The player's frames are: ${playerTurns.toList}")
  println(s"The scores for each frame are $frameScores")
  println(s"The total player score is ${frameScores.sum}")
}

