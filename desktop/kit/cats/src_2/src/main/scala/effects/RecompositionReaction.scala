package gui4s.desktop.kit.cats
package effects

import catnip.syntax.applicative.given
import cats.*
import cats.syntax.all.* 
import cats.effect.IO

type RecompositionReaction = gui4s.desktop.kit.common.effects.RecompositionReaction[IO]

object RecompositionReaction:
  def empty : RecompositionReaction = ().pure[IO]

  def run(recomposition : RecompositionReaction) : IO[Unit] = recomposition

  def lift[Value](value : IO[Value]) : RecompositionReaction = value.as(())

  def raiseError[Error](error : Throwable) : RecompositionReaction =
    IO.raiseError(error)
  end raiseError

  given Monoid[RecompositionReaction] = applicativesAreMonoids
end RecompositionReaction
