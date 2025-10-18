package gui4s.desktop.kit.zio
package effects

import cats.syntax.all.*
import gui4s.core.geometry.Point2d
import gui4s.core.widget.Path
import gui4s.core.widget.Path.given
import glfw4s.shared.KeyAction.{*, given}
import gui4s.glfw.{GlfwCallbacks, KeyAction, KeyModes}
import zio.*
import zio.interop.catz.*

type DownEvent = gui4s.desktop.kit.common.effects.DownEvent

object DownEvent:
  def eventOfferingCallbacks(offerEvent: DownEvent => Task[Unit]) : GlfwCallbacks[Task[Unit], Float] =
    gui4s.desktop.kit.common.effects.DownEvent.eventOfferingCallbacks(offerEvent)
  end eventOfferingCallbacks

  def extractMouseClickEvent(downEvent : DownEvent) : Option[Unit] =
    gui4s.desktop.kit.common.effects.DownEvent.extractMouseClickEvent(downEvent)
  end extractMouseClickEvent
  
  def catchExternalEvent(expectedPath : Path, event : DownEvent) : Option[Any] =
    gui4s.desktop.kit.common.effects.DownEvent.catchExternalEvent(expectedPath, event)
  end catchExternalEvent
end DownEvent

