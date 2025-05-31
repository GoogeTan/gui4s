package catnip

type Bi[Trait[F[_]]] = [F[_, _]] =>> [A] => () => Trait[F[A, *]]

