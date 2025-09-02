package gui4s.desktop.kit.zio
package effects

import zio.*
import zio.interop.catz.*
import cats.*
import cats.syntax.all.*
import catnip.syntax.applicative.given 

type RecompositionReaction = gui4s.desktop.kit.effects.RecompositionReaction[Task]

object RecompositionReaction:
  def empty : RecompositionReaction = ().pure[Task]

  def run(recomposition : RecompositionReaction) : Task[Unit] = recomposition

  def lift[Value](value : Task[Value]) : RecompositionReaction = value.as(())

  def raiseError[Error](error : Throwable) : RecompositionReaction =
    ZIO.fail(error)
  end raiseError

  given Monoid[RecompositionReaction] = applicativesAreMonoids
end RecompositionReaction
