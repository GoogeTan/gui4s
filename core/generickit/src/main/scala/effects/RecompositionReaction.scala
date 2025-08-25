package gui4s.core.kit
package effects

import catnip.syntax.all.given
import cats.*
import cats.syntax.all.*

type RecompositionReaction[F[_]] = F[Unit]

class RecompositionReactionOps[F[_] : Applicative]:
  final def empty : RecompositionReaction[F] = ().pure[F]

  final def run(recomposition : RecompositionReaction[F]) : F[Unit] = recomposition

  final def lift[T](value : F[T]) : RecompositionReaction[F] = value.as(())

  final given Monoid[RecompositionReaction[F]] = summon
end RecompositionReactionOps

object RecompositionReaction:
  def apply[F[_] : Applicative]: RecompositionReactionOps[F] = RecompositionReactionOps[F]()
end RecompositionReaction