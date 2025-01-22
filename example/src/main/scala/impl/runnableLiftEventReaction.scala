package me.katze.gui4s.example
package impl

import me.katze.gui4s.widget.EventResult
import me.katze.gui4s.widget.library.LiftEventReaction
import me.katze.gui4s.widget.stateful.EventReaction

given runnableLiftEventReaction[Task]: LiftEventReaction[[A, B] =>> EventResult[Task, A, B], Task] with
  override def lift[A, B](reaction: EventReaction[Task, A, B]): EventResult[Task, A, B] =
    EventResult(
      reaction.newState,
      reaction.parentEvent,
      reaction.ios
    )
  end lift
end runnableLiftEventReaction
