package gui4s.desktop.kit.zio
package effects

import gui4s.core.kit.effects as generic_effects
import zio.*
import zio.interop.catz.*

type RecompositionReaction = generic_effects.RecompositionReaction[Task]

object RecompositionReaction extends generic_effects.RecompositionReactionOps[Task]
