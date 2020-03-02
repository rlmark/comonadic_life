package conway

import conway.Main.Coordinates

sealed trait Patterns {
  val shape: Map[Coordinates, Int]
  val value: Int
}

object Patterns {
  case object Glider extends Patterns {
    val shape: Map[Coordinates, Int] = Map(
      (1, 0) -> 1,
      (2, 1) -> 1,
      (0, 2) -> 1,
      (1, 2) -> 1,
      (2, 2) -> 1,
    )
    val value: Int = 0
  }

  case object Blinker extends Patterns {
    val shape: Map[Coordinates, Int] = Map(
      (0, 0) -> 1,
      (1, 0) -> 1,
      (2, 0) -> 1
    )
    val value: Int = 1
  }

  case object Beacon extends Patterns {
    val shape: Map[Coordinates, Int] = Map(
      (0, 0) -> 1,
      (1, 0) -> 1,
      (0, 1) -> 1,
      (3, 2) -> 1,
      (2, 3) -> 1,
      (3, 3) -> 1
    )
    val value: Int = 2
  }

  case object Toad extends Patterns {
    val shape: Map[Coordinates, Int] = Map(
      (0, 2) -> 1,
      (0, 3) -> 1,
      (1, 1) -> 1,
      (0, 1) -> 1,
      (1, 2) -> 1,
      (1, 0) -> 1
    )
    val value: Int = 3
  }

  case object DieHard extends Patterns {
    val shape: Map[Coordinates, Int] = Map(
      (5, 1) -> 1,
      (4, 0) -> 1,
      (4, 1) -> 1,
      (3, 6) -> 1,
      (5, 7) -> 1,
      (5, 5) -> 1,
      (5, 6) -> 1
    )
    val value: Int = 4
  }

  case class UnknownShape(value: Int) extends Patterns {
    val shape: Map[Coordinates, Int]= Map.empty
  }

  val patterns: List[Patterns] = List(Glider, Blinker, Beacon, Toad, DieHard)

  def apply(int: Int): Patterns = {
    patterns.find(pattern => pattern.value == int).getOrElse(UnknownShape(int))
  }

}

sealed trait PatternsC {
  val shape: Map[typeclasses.data.Coordinates, Int]
  val value: Int
}

object PatternsC {
  case object Glider extends PatternsC {
    val shape: Map[typeclasses.data.Coordinates, Int] = Map(
      typeclasses.data.Coordinates(1, 0) -> 1,
      typeclasses.data.Coordinates(2, 1) -> 1,
      typeclasses.data.Coordinates(0, 2) -> 1,
      typeclasses.data.Coordinates(1, 2) -> 1,
      typeclasses.data.Coordinates(2, 2) -> 1,
    )
    val value: Int = 0
  }

  case object Blinker extends PatternsC {
    val shape: Map[typeclasses.data.Coordinates, Int] = Map(
      typeclasses.data.Coordinates(0, 0) -> 1,
      typeclasses.data.Coordinates(1, 0) -> 1,
      typeclasses.data.Coordinates(2, 0) -> 1
    )
    val value: Int = 1
  }

  case object Beacon extends PatternsC {
    val shape: Map[typeclasses.data.Coordinates, Int] = Map(
      typeclasses.data.Coordinates(0, 0) -> 1,
      typeclasses.data.Coordinates(1, 0) -> 1,
      typeclasses.data.Coordinates(0, 1) -> 1,
      typeclasses.data.Coordinates(3, 2) -> 1,
      typeclasses.data.Coordinates(2, 3) -> 1,
      typeclasses.data.Coordinates(3, 3) -> 1
    )
    val value: Int = 2
  }

  case object Toad extends PatternsC {
    val shape: Map[typeclasses.data.Coordinates, Int] = Map(
      typeclasses.data.Coordinates(0, 2) -> 1,
      typeclasses.data.Coordinates(0, 3) -> 1,
      typeclasses.data.Coordinates(1, 1) -> 1,
      typeclasses.data.Coordinates(0, 1) -> 1,
      typeclasses.data.Coordinates(1, 2) -> 1,
      typeclasses.data.Coordinates(1, 0) -> 1
    )
    val value: Int = 3
  }

  case object DieHard extends PatternsC {
    val shape: Map[typeclasses.data.Coordinates, Int] = Map(
      typeclasses.data.Coordinates(5, 1) -> 1,
      typeclasses.data.Coordinates(4, 0) -> 1,
      typeclasses.data.Coordinates(4, 1) -> 1,
      typeclasses.data.Coordinates(3, 6) -> 1,
      typeclasses.data.Coordinates(5, 7) -> 1,
      typeclasses.data.Coordinates(5, 5) -> 1,
      typeclasses.data.Coordinates(5, 6) -> 1
    )
    val value: Int = 4
  }

  case class UnknownShape(value: Int) extends PatternsC {
    val shape: Map[typeclasses.data.Coordinates, Int]= Map.empty
  }

  val patterns: List[PatternsC] = List(Glider, Blinker, Beacon, Toad, DieHard)

  def apply(int: Int): PatternsC = {
    patterns.find(pattern => pattern.value == int).getOrElse(UnknownShape(int))
  }

}
