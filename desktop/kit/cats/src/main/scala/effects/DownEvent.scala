package gui4s.desktop.kit.cats
package effects

import gui4s.core.geometry.Point2d
import gui4s.glfw.KeyAction.Press
import gui4s.glfw.{GlfwCallbacks, KeyAction, KeyModes}
import gui4s.core.widget.Path

import scala.reflect.Typeable

@SuppressWarnings(Array("org.wartremover.warts.Any"))
enum DownEvent:
  case WindowResized
  case MouseClick(button: Int, action: KeyAction, mods: KeyModes)
  case MouseMove(position: Point2d[Float])
  case KeyPress(key: Int, scancode: Int, action: KeyAction, mods: KeyModes)
  case Scrolled(xoffset: Float, yoffset: Float)
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  case ExternalEventForWidget(destination: Path, event: Any)
end DownEvent

object DownEvent:
  def eventOfferingCallbacks[F](offerEvent: DownEvent => F) : GlfwCallbacks[F, Float] =
    GlfwCallbacks(
      onWindowResized = _ => offerEvent(DownEvent.WindowResized),
      onMouseClick = (button, action, mods) => offerEvent(DownEvent.MouseClick(button, action, mods)),
      onMouseMove = newPosition => offerEvent(DownEvent.MouseMove(newPosition)),
      onKeyPress = (key, scancode, action, mods) => offerEvent(DownEvent.KeyPress(key, scancode, action, mods)),
      onScroll = (xoffset, yoffset) => offerEvent(DownEvent.Scrolled(xoffset, yoffset))
    )
  end eventOfferingCallbacks

  def extractMouseClickEvent(downEvent : DownEvent) : Option[Unit] =
    downEvent match
      case DownEvent.MouseClick(_, action, _) if action == Press =>
        Some(()) // TODO ClickHandlerDownEvent(button, action, mods))
      case _ => None
  end extractMouseClickEvent
  
  def catchExternalEvent[ExpectedType : Typeable, Error](expectedPath : Path, event : DownEvent, errorIfMistyped : Any => Error) : Option[Either[Error, ExpectedType]] =
    event match 
      case DownEvent.ExternalEventForWidget(taskPath, event : ExpectedType) if taskPath == expectedPath =>
        Some(Right(event))
      case DownEvent.ExternalEventForWidget(taskPath, valueFound : Any) if taskPath == expectedPath =>
        Some(Left(errorIfMistyped(valueFound)))
      case _ =>
        None 
    end match
  end catchExternalEvent
end DownEvent

