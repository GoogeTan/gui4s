package me.katze.gui4s.example
package api.effects

import me.katze.gui4s.geometry.Point2d
import me.katze.gui4s.glfw.KeyAction.Press
import me.katze.gui4s.glfw.{KeyAction, KeyModes}
import me.katze.gui4s.widget.Path

import scala.reflect.Typeable

@SuppressWarnings(Array("org.wartremover.warts.Any"))
enum SkijaDownEvent[+MeasurementUnit]:
  case WindowResized extends SkijaDownEvent[Nothing]
  case MouseClick(button: Int, action: KeyAction, mods: KeyModes)
  case MouseMove(position: Point2d[MeasurementUnit])
  case KeyPress(key: Int, scancode: Int, action: KeyAction, mods: KeyModes)
  case Scrolled(xoffset: MeasurementUnit, yoffset: MeasurementUnit)
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  case ExternalEventForWidget(destination: Path, event: Any)
end SkijaDownEvent


object SkijaDownEvent:
  def eventOfferingCallbacks[F, MeasurementUnit](offerEvent: SkijaDownEvent[MeasurementUnit] => F):  skija.SkijaBackend.GlfwCallbacks[F, MeasurementUnit] =
    skija.SkijaBackend.GlfwCallbacks(
      onWindowResized = _ => offerEvent(SkijaDownEvent.WindowResized),
      onMouseClick = (button, action, mods) => offerEvent(SkijaDownEvent.MouseClick(button, action, mods)),
      onMouseMove = newPosition => offerEvent(SkijaDownEvent.MouseMove(newPosition)),
      onKeyPress = (key, scancode, action, mods) => offerEvent(SkijaDownEvent.KeyPress(key, scancode, action, mods)),
      onScroll = (xoffset, yoffset) => offerEvent(SkijaDownEvent.Scrolled(xoffset, yoffset))
    )
  end eventOfferingCallbacks

  def extractMouseClickEvent[MeasurementUnit](downEvent : SkijaDownEvent[MeasurementUnit]) : Option[Unit] =
    downEvent match
      case SkijaDownEvent.MouseClick(_, action, _) if action == Press =>
        Some(()) // TODO ClickHandlerDownEvent(button, action, mods))
      case _ => None
  end extractMouseClickEvent
  
  def catchExternalEvent[ExpectedType : Typeable, MeasurementUnit, Error](expectedPath : Path, event : SkijaDownEvent[MeasurementUnit], errorIfMistyped : Any => Error) : Option[Either[Error, ExpectedType]] =
    event match 
      case SkijaDownEvent.ExternalEventForWidget(taskPath, event : ExpectedType) if taskPath == expectedPath =>
        Some(Right(event))
      case SkijaDownEvent.ExternalEventForWidget(taskPath, valueFound : Any) if taskPath == expectedPath =>
        Some(Left(errorIfMistyped(valueFound)))
      case _ =>
        None 
    end match
  end catchExternalEvent
end SkijaDownEvent

