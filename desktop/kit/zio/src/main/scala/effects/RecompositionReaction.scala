package gui4s.desktop.kit.zio
package effects

import gui4s.core.kit.effects as generic_effects
import zio.*
import zio.interop.cats.*

type RecompositionReaction = generic_effects.RecompositionReaction[UIO]

object RecompositionReaction extends generic_effects.RecompositionReactionOps[UIO]
