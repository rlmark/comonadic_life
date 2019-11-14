package instances

case class Store[S, A](query: S => A)(val index: S)
