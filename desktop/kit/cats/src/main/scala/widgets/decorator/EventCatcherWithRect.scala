package gui4s.desktop.kit.cats
package widgets.decorator

import effects.*
import effects.Place.given
import effects.Update.given
import widgets.*

import cats.Monad
import cats.effect.IO
import gui4s.desktop.widget.library.*
import gui4s.desktop.widget.library.decorator.{EventCatcherWithRect, eventCatcherWithRect}

def eventCatcher[Event]: EventCatcherWithRect[
  DesktopWidget[Event],
  Update[Event, Boolean],
  InnerPlace[DesktopPlacedWidget[Event]],
  DownEvent
] =  gui4s.desktop.kit.widgets.decorator.eventCatcher[IO, Event]