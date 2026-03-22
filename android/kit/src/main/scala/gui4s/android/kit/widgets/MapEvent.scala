package gui4s.android.kit.widgets

import gui4s.core.widget.library.MapEvent
import gui4s.android.kit.effects.*
import gui4s.desktop.widget.library.decorator.mapUpdate as genericMapUpdate

import cats.effect.IO
import cats.Monad
import gui4s.core.widget.library.MapEvent
import gui4s.android.kit.effects.*
import gui4s.desktop.widget.library.decorator.mapUpdate as genericMapUpdate

def mapEventWidget : MapEvent[AndroidWidget] =
  [Event, NewEvent] => f => original =>
    genericMapUpdate[UpdateC[Event], UpdateC[NewEvent], PlacementEffect, Situated, Draw, RecompositionReaction](
      original,
      Update.mapEvents[Event, NewEvent](f)
    )
end mapEventWidget


extension[Event](value : AndroidWidget[Event])
  def mapEvent[NewEvent](f : Event => NewEvent) : AndroidWidget[NewEvent] =
    mapEventWidget(f)(value)
  end mapEvent
end extension
