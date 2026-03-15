package gui4s.desktop.kit
package effects

import cats.Applicative
import cats.MonadError
import cats.Monoid
import cats.effect.IO

type RecompositionReaction = gui4s.core.kit.effects.RecompositionReaction[IO]

object RecompositionReaction:
  export gui4s.core.kit.effects.RecompositionReaction.{*, given}

  def empty: RecompositionReaction =
    gui4s.core.kit.effects.RecompositionReaction.empty

  def run(recomposition: RecompositionReaction): IO[Unit] =
    gui4s.core.kit.effects.RecompositionReaction.run(recomposition)

  def lift[Value](value: IO[Value]): RecompositionReaction =
    gui4s.core.kit.effects.RecompositionReaction.lift(value)

  def raiseError(error: Throwable): RecompositionReaction =
    gui4s.core.kit.effects.RecompositionReaction.raiseError[IO, Throwable](error)
  end raiseError

  given Monoid[RecompositionReaction] = summon
end RecompositionReaction
