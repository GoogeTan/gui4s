package gui4s.desktop.kit
package effects

import cats.syntax.all._

import gui4s.core.widget.Path
import gui4s.core.widget.Path.given

@SuppressWarnings(Array("org.wartremover.warts.Any"))
enum DownEvent:
  case WindowShouldBeRedrawn
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
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

