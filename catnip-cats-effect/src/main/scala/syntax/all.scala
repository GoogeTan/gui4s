package catnip.cats.effect
package syntax

import cats.effect.MonadCancel

object all:
  export fails.{*, given}
  
  type BiMonadCancel[F[_, _]] = [Error] => () => MonadCancel[F[Error, *], Error]
  
  given[F[_, _] : BiMonadCancel as b, Error]: MonadCancel[F[Error, *], Error] = b()
end all

