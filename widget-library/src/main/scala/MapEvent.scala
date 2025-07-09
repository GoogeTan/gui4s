package me.katze.gui4s.widget.library

import cats.syntax.all.*
import cats.{Bifunctor, Functor}
import me.katze.gui4s.widget.handle.mapEventHandle

trait MapEvent[Widget[_]]:
  def mapEvent[A, B](value : Widget[A])(f : A => B) : Widget[B]
end MapEvent

// TODO for some reason name mapEvent without 2 is not compiled on mac os
final class mapEvent2[
  Update[_, _],
  Place[_] : Functor,
  Draw,
  RecompositionReaction,
  HandleableEvent,
](
  mapEvent : [T, A, B] => (A => B) => Update[A, T] => Update[B, T] 
) extends MapEvent[
  [Event] =>> Place[Widget_[Update[Event, *], Place, Draw, RecompositionReaction, HandleableEvent]]
]:
  override def mapEvent[A, B](
                               value: Place[Widget_[Update[A, *], Place, Draw, RecompositionReaction, HandleableEvent]]
                             )(
                               f: A => B
                              ): Place[Widget_[Update[B, *], Place, Draw, RecompositionReaction, HandleableEvent]] =
    value.map(
      widget =>
        widget.copy(
          valueHandlesEvent =
            mapEventHandle(widget.valueHandlesEvent)(mapEvent(f))
        )
    )
  end mapEvent
end mapEvent2
