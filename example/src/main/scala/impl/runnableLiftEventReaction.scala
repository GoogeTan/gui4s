package me.katze.gui4s.example
package impl

import me.katze.gui4s.widget.{EventResult, LiftEventReaction}
import me.katze.gui4s.widget.stateful.EventReaction

given runnableLiftEventReaction: LiftEventReaction[EventResult] with
  override def lift[A, B](reaction: EventReaction[A, B, Nothing]): EventResult[A, B] =
    EventResult(
      reaction.newState,
      reaction.parentEvent,
    )
  end lift
end runnableLiftEventReaction
