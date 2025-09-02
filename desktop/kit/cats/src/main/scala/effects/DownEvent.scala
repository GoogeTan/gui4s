package gui4s.desktop.kit.cats
package effects

import cats.syntax.all.*
import cats.effect.IO
import gui4s.core.geometry.Point2d
import gui4s.core.widget.Path
import gui4s.core.widget.Path.given
import gui4s.glfw.KeyAction.{Press, given}
import gui4s.glfw.{GlfwCallbacks, KeyAction, KeyModes}

type DownEvent = gui4s.desktop.kit.common.effects.DownEvent

object DownEvent:
  def eventOfferingCallbacks(offerEvent: DownEvent => IO[Unit]) : GlfwCallbacks[IO[Unit], Float] =
    gui4s.desktop.kit.common.effects.DownEvent.eventOfferingCallbacks(offerEvent)
  end eventOfferingCallbacks

  def extractMouseClickEvent(downEvent : DownEvent) : Option[Unit] =
    gui4s.desktop.kit.common.effects.DownEvent.extractMouseClickEvent(downEvent)
  end extractMouseClickEvent
  
  def catchExternalEvent(expectedPath : Path, event : DownEvent) : Option[Any] =
    gui4s.desktop.kit.common.effects.DownEvent.catchExternalEvent(expectedPath, event)
  end catchExternalEvent
end DownEvent

