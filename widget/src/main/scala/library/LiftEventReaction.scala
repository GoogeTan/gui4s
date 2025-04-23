package me.katze.gui4s.widget
package library

import stateful.EventReaction

trait LiftEventReaction[
  Update[+_, +_],
]:
  def lift[A, B](reaction: EventReaction[A, B, Nothing]): Update[A, B]
end LiftEventReaction
