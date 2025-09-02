package gui4s.desktop.kit
package common.effects

type RecompositionReaction[IO[_]] = gui4s.core.kit.effects.RecompositionReaction[IO]

object RecompositionReaction:
  export gui4s.core.kit.effects.RecompositionReaction.{*, given}
end RecompositionReaction
