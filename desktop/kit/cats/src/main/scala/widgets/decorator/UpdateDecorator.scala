package gui4s.desktop.kit.cats
package widgets.decorator

import effects.*
import effects.OuterPlace.given
import effects.Update.given
import widgets.*

import cats.*
import cats.effect.IO
import gui4s.desktop.widget.library.decorator.*

def updateDecorator[Event]: UpdateDecorator[
  UpdateC[Event],
  OuterPlace,
  InnerPlace[DesktopPlacedWidget[Event]],
  DownEvent
] = gui4s.desktop.kit.common.widgets.decorator.updateDecorator[IO, Event]

