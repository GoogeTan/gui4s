package gui4s.android.kit.effects

type RecompositionReaction[IO[_]] = gui4s.core.kit.effects.RecompositionReaction[IO]

object RecompositionReaction:
  export gui4s.core.kit.effects.RecompositionReaction.{*, given}
end RecompositionReaction
