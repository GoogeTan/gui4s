package gui4s.desktop.kit.zio
package widgets.decorator

import effects.*
import effects.OuterPlace.given
import effects.Update.given
import widgets.*

import cats.*
import zio.*
import zio.interop.catz.*
import gui4s.desktop.widget.library.decorator.*

def updateDecorator[Event]: UpdateDecorator[
  UpdateC[Event],
  OuterPlace,
  InnerPlace[DesktopPlacedWidget[Event]],
  DownEvent
] = gui4s.desktop.kit.widgets.decorator.updateDecorator[Task, Event]

