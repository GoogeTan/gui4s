package gui4s.desktop.kit.cats
package widgets.decorator

import effects.*
import widgets.*

import cats.effect.IO
import gui4s.desktop.widget.library.decorator.EventCatcherWithRect

def eventCatcher[Event]: EventCatcherWithRect[
  DesktopWidget[Event],
  Update[Event, Boolean],
  InnerPlace[DesktopPlacedWidget[Event]],
  DownEvent
] =  gui4s.desktop.kit.common.widgets.decorator.eventCatcher[IO, Event]