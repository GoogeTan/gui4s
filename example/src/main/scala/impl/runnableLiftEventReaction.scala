package me.katze.gui4s.example
package impl

import me.katze.gui4s.example.EventResult
import me.katze.gui4s.widget.{EventReaction, LiftEventReaction}

given runnableLiftEventReaction: LiftEventReaction[EventResult] with
  override def lift[A, B](reaction: EventReaction[A, B, Nothing]): EventResult[A, B] =
    EventResult(
      reaction.newState,
      reaction.parentEvent,
    )
  end lift
end runnableLiftEventReaction
