package gui4s.desktop.kit.zio
package widgets.decorator

import effects.*
import widgets.*

import zio.*
import zio.interop.catz.*
import gui4s.desktop.widget.library.decorator.EventCatcherWithRect

def eventCatcher[Event]: EventCatcherWithRect[
  DesktopWidget[Event],
  Update[Event, Boolean],
  InnerPlace[DesktopPlacedWidget[Event]],
  DownEvent
] =  gui4s.desktop.kit.common.widgets.decorator.eventCatcher[Task, Event]