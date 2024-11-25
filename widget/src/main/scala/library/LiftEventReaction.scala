package me.katze.gui4s.widget
package library

import me. katze. gui4s. widget. stateful.EventReaction

trait LiftEventReaction[
  Update[+_, +_],
  WidgetTask,
]:
  def lift[A, B](reaction: EventReaction[WidgetTask, A, B]): Update[A, B]
end LiftEventReaction
