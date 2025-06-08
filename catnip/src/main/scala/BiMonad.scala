package catnip

import cats.Monad

type BiMonad[F[_, _]] = Bi[Monad][F]


