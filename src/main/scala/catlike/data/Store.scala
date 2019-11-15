package catlike.data

case class Store[S, A](query: S => A, index: S)
