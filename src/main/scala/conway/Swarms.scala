package conway

object Swarms {
  val glider: Map[(Int, Int), Int] = Map(
    (1, 0) -> 1,
    (2, 1) -> 1,
    (0, 2) -> 1,
    (1, 2) -> 1,
    (2, 2) -> 1,
  )

  val blinker: Map[(Int, Int), Int] = Map(
    (0, 0) -> 1,
    (1, 0) -> 1,
    (2, 0) -> 1
  )

  val beacon: Map[(Int, Int), Int] = Map(
    (0, 0) -> 1,
    (1, 0) -> 1,
    (0, 1) -> 1,
    (3, 2) -> 1,
    (2, 3) -> 1,
    (3, 3) -> 1
  )

  val toad: Map[(Int, Int), Int] = Map(
    (0, 2) -> 1,
    (0, 3) -> 1,
    (1, 1) -> 1,
    (0, 1) -> 1,
    (1, 2) -> 1,
    (1, 0) -> 1
  )

  val dieHard: Map[(Int, Int), Int] = Map(
    (5,1) -> 1,
    (4,0) -> 1,
    (4,1) -> 1,
    (3,6) -> 1,
    (5,7) -> 1,
    (5,5) -> 1,
    (5,6) -> 1
  )

}
