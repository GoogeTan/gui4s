package gui4s.desktop.kit.cats
package widgets.decorator

import effects.*
import widgets.*

import cats.*
import cats.data.EitherT
import cats.effect.IO
import glfw4s.core.types.GlfwError
import gui4s.desktop.widget.library.decorator.*

def updateDecorator[Event]: UpdateDecorator[
  UpdateC[Event],
  OuterPlace,
  InnerPlace[DesktopPlacedWidget[Event]],
  DownEvent
] = gui4s.desktop.kit.common.widgets.decorator.updateDecorator[EitherT[IO, GlfwError, *], Event]

