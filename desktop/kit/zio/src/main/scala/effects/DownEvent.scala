package gui4s.desktop.kit.zio
package effects

import gui4s.core.widget.Path

type DownEvent = gui4s.desktop.kit.common.effects.DownEvent

object DownEvent:
  def extractMouseClickEvent(downEvent : DownEvent) : Option[Unit] =
    gui4s.desktop.kit.common.effects.DownEvent.extractMouseClickEvent(downEvent)
  end extractMouseClickEvent
  
  def catchExternalEvent(expectedPath : Path, event : DownEvent) : Option[Any] =
    gui4s.desktop.kit.common.effects.DownEvent.catchExternalEvent(expectedPath, event)
  end catchExternalEvent
end DownEvent

