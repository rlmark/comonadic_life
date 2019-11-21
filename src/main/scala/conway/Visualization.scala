package conway


trait Visualization {
  val alive: String
  val background: String
}

object Visualization {
  object Forest extends Visualization {
    val alive = "\uD83E\uDD84"
    val background = "\uD83C\uDF32"
  }

  object Ocean extends Visualization {
    val alive = "\uD83D\uDC19"
    val background = "\uD83C\uDF0A"
  }
}
