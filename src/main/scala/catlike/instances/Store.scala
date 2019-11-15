package catlike.instances

case class Store[S, A](query: S => A, index: S)
