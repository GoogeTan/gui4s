package gui4s.core.kit
package effects

import catnip.syntax.all.given
import cats._
import cats.syntax.all._

type RecompositionReaction[F[_]] = F[Unit]

object RecompositionReaction:
  def empty[F[_] : Applicative] : RecompositionReaction[F] = ().pure[F]

  def run[F[_] : Applicative](recomposition : RecompositionReaction[F]) : F[Unit] = recomposition

  def lift[F[_] : Applicative, Value](value : F[Value]) : RecompositionReaction[F] = value.as(())

  def raiseError[F[_] : Applicative, Error](using M : MonadError[F, Error])(error : Error) : RecompositionReaction[F] =
    M.raiseError(error)
  end raiseError
  
  given[F[_] : Applicative]: Monoid[RecompositionReaction[F]] = summon
end RecompositionReaction

