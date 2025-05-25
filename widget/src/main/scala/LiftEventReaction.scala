package me.katze.gui4s.widget

trait LiftEventReaction[
  Update[+_, +_],
]:
  def lift[A, B](reaction: EventReaction[A, B, Nothing]): Update[A, B]
end LiftEventReaction
