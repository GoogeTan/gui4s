package gui4s.desktop.kit
package widgets

import cats.effect._

import gui4s.core.widget.library.MapEvent

import gui4s.desktop.kit.effects._
import gui4s.desktop.widget.library.decorator.{mapUpdate => genericMapUpdate}

def mapEventWidget : MapEvent[DesktopWidget] =
  [Event, NewEvent] => f => original =>
    genericMapUpdate[UpdateC[Event], UpdateC[NewEvent], PlacementEffect, Situated, Draw, RecompositionReaction, DownEvent](
      original,
      Update.mapEvents[Event, NewEvent](f)
    )
end mapEventWidget


extension[Event](value : DesktopWidget[Event])
  def mapEvent[NewEvent](f : Event => NewEvent) : DesktopWidget[NewEvent] =
    mapEventWidget(f)(value)
  end mapEvent
end extension
