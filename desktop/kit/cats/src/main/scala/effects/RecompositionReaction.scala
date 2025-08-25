package gui4s.desktop.kit.cats
package effects

import cats.effect.IO
import gui4s.core.kit.effects as generic_effects

type RecompositionReaction = generic_effects.RecompositionReaction[IO]

object RecompositionReaction extends generic_effects.RecompositionReactionOps[IO]