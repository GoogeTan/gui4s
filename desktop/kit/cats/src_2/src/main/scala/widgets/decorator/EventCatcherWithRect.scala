package gui4s.desktop.kit.cats
package widgets.decorator

import effects.*
import widgets.*

import cats.data.EitherT
import cats.effect.IO
import glfw4s.core.types.GlfwError
import gui4s.desktop.widget.library.decorator.EventCatcherWithRect

def eventCatcher[Event]: EventCatcherWithRect[
  DesktopWidget[Event],
  Update[Event, Boolean],
  InnerPlace[DesktopPlacedWidget[Event]],
  DownEvent
] =  gui4s.desktop.kit.common.widgets.decorator.eventCatcher[EitherT[IO, GlfwError, *], Event]