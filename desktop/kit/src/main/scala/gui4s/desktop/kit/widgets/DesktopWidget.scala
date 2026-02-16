package gui4s.desktop.kit
package widgets

import gui4s.core.kit.effects.RecompositionReaction

import gui4s.desktop.kit.effects._
import gui4s.desktop.widget.library.Widget
import cats.effect.*

type DesktopPlacedWidget[Event] = Widget[UpdateC[IO, Event], PlaceC[IO], Draw[IO], RecompositionReaction[IO], DownEvent]
type DesktopWidget[Event] = Place[IO, DesktopPlacedWidget[Event]]
