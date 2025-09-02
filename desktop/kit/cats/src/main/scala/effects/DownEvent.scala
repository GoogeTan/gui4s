package gui4s.desktop.kit.cats
package effects

import cats.effect.IO
import gui4s.core.widget.Path
import gui4s.glfw.GlfwCallbacks

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

