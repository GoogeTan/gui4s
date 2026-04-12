package gui4s.desktop.kit
package effects

import cats.syntax.all.*
import gui4s.core.geometry.Point2d
import gui4s.core.widget.Path
import gui4s.core.widget.Path.given

enum DownEvent:
  case ExternalEventForWidget(destination: Path, event: Any)
  case UserEvent(event: Any)
end DownEvent

object DownEvent:
  def catchExternalEvent(expectedPath : Path, event : DownEvent) : Option[Any] =
    event match 
      case DownEvent.ExternalEventForWidget(taskPath, event : Any) if taskPath === expectedPath =>
        Some[Any](event)
      case _ =>
        None 
    end match
  end catchExternalEvent
end DownEvent

