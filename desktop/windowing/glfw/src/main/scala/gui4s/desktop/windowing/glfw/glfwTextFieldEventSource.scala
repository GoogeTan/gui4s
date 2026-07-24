package gui4s.desktop.windowing.glfw

import catnip.syntax.all.*
import cats.Monad
import cats.effect.IO
import cats.effect.std.Queue
import cats.syntax.all.*
import glfw4s.core.pure.PurePostInit
import glfw4s.core.{GlfwConstants, KeyAction}
import glfw4s.jna.bindings.types.{GLFWcursor, GLFWmonitor, GLFWwindow}
import gui4s.core.geometry.{Point2d, Rect}
import gui4s.core.layout.Sized
import gui4s.core.widget.Path
import gui4s.core.widget.library.decorator.Decorator
import gui4s.core.widget.library.textfield.TextFieldEvent
import gui4s.desktop.kit.effects.{DownEvent, Update, UpdateC}
import gui4s.desktop.kit.widgets.DesktopWidget
import gui4s.desktop.kit.widgets.decorator.eventCatcher
import io.github.humbleui.skija.paragraph.Paragraph

import scala.math.min

enum TextInputOuterEvent:
  case CharInputEvent(key : Int)
  case Backspace
  case Enter
  case Left(shift : Boolean = false)
  case Right(shift : Boolean = false)
  case Up(shift : Boolean = false)
  case Down(shift : Boolean = false)
  case LeftMouseClick(x : Float, y : Float, press : Boolean)
  case ClipbordPaste
  case ClipbordCopy
  case ClipbordCut
  case SelectAll
  case FocusedOn(path : Path)
end TextInputOuterEvent

@SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
def textFieldEventSource(
  glfw: PurePostInit[IO, IO[Unit], GLFWmonitor, GLFWwindow, GLFWcursor, Int],
  window: GLFWwindow,
  eventBus: Queue[IO, DownEvent]
): IO[Unit] =
  glfw.addCharCallback(window, (_, key) =>
    eventBus.offer(DownEvent.UserEvent(TextInputOuterEvent.CharInputEvent(key)))
  ) *>
    glfw.addKeyCallback(window, (_, key, _, action, mods) =>//TODO add scancode to the event
      if action =!= KeyAction.Press then
        IO.unit
      else
        key match
          case GlfwConstants.GLFW_KEY_BACKSPACE =>
            eventBus.offer(DownEvent.UserEvent(TextInputOuterEvent.Backspace))
          case GlfwConstants.GLFW_KEY_ENTER =>
            eventBus.offer(DownEvent.UserEvent(TextInputOuterEvent.Enter))
          case GlfwConstants.GLFW_KEY_LEFT =>
            eventBus.offer(DownEvent.UserEvent(TextInputOuterEvent.Left(mods.shiftActive)))
          case GlfwConstants.GLFW_KEY_RIGHT =>
            eventBus.offer(DownEvent.UserEvent(TextInputOuterEvent.Right(mods.shiftActive)))
          case GlfwConstants.GLFW_KEY_UP =>
            eventBus.offer(DownEvent.UserEvent(TextInputOuterEvent.Up(mods.shiftActive)))
          case GlfwConstants.GLFW_KEY_DOWN =>
            eventBus.offer(DownEvent.UserEvent(TextInputOuterEvent.Down(mods.shiftActive)))
          case GlfwConstants.GLFW_KEY_V if mods.controlActive =>
            eventBus.offer(DownEvent.UserEvent(TextInputOuterEvent.ClipbordPaste))
          case GlfwConstants.GLFW_KEY_C if mods.controlActive =>
            eventBus.offer(DownEvent.UserEvent(TextInputOuterEvent.ClipbordCopy))
          case GlfwConstants.GLFW_KEY_X if mods.controlActive =>
            eventBus.offer(DownEvent.UserEvent(TextInputOuterEvent.ClipbordCut))
          case GlfwConstants.GLFW_KEY_A if mods.controlActive =>
            eventBus.offer(DownEvent.UserEvent(TextInputOuterEvent.SelectAll))
          case _ =>
            IO.unit
        end match
      end if
    ) *> glfw.addMouseButtonCallback(window, (_, button, action, _) =>
    if button == GlfwConstants.GLFW_MOUSE_BUTTON_LEFT && action =!= KeyAction.Repeat then
      for
        monitor <- glfw.getPrimaryMonitor
        (scaleX, scaleY) <- glfw.getMonitorContentScale(monitor.get)
        (x, y) <- glfw.getCursorPos(window)
        _ <- eventBus.offer(
          DownEvent.UserEvent(
            TextInputOuterEvent.LeftMouseClick((x * scaleX).toFloat, (y * scaleY).toFloat, action === KeyAction.Press)
          )
        )
      yield ()
    else
      IO.unit
    end if
  )
end textFieldEventSource

def textFieldEventCatcher(
  glfw: PurePostInit[IO, IO[Unit], GLFWmonitor, GLFWwindow, GLFWcursor, Int],
  window: GLFWwindow,
  minimalClickableAreaSize: Rect[Float],
  requestFocus: Path => Update[TextFieldEvent, Unit]
)(
  sizedParagraph: Sized[Rect[Float], Paragraph],
): Decorator[DesktopWidget[TextFieldEvent]] =
  val emitEvent: List[TextFieldEvent] => Update[TextFieldEvent, Boolean] = events =>
    Update.emitEvents[TextFieldEvent](events).as(true)

  def convertFocusedEvent(textFieldTextAreaSize: Rect[Float], currentPath: Path): TextInputOuterEvent => Update[TextFieldEvent, List[TextFieldEvent]] = {
    case TextInputOuterEvent.CharInputEvent(key) => TextFieldEvent.CharInput(key.toChar).one.pure
    case TextInputOuterEvent.Backspace => TextFieldEvent.Backspace.one.pure
    case TextInputOuterEvent.Enter => TextFieldEvent.CharInput('\n').one.pure
    case TextInputOuterEvent.Left(shift) => TextFieldEvent.GoLeft(shift).one.pure
    case TextInputOuterEvent.Right(shift) => TextFieldEvent.GoRight(shift).one.pure
    case TextInputOuterEvent.Up(shift) => TextFieldEvent.GoUp(shift).one.pure
    case TextInputOuterEvent.Down(shift) => TextFieldEvent.GoDown(shift).one.pure
    case TextInputOuterEvent.ClipbordPaste =>
      Update.liftK[TextFieldEvent](
        glfw
          .getClipboardString(window)
          .map(_.toList.map(TextFieldEvent.ClipboardPaste(_)))
      )
    case TextInputOuterEvent.ClipbordCopy =>
      TextFieldEvent.ClipboardCopy.one.pure
    case TextInputOuterEvent.ClipbordCut =>
      TextFieldEvent.ClipboardCut.one.pure
    case TextInputOuterEvent.SelectAll =>
      TextFieldEvent.SelectAll.one.pure
    case TextInputOuterEvent.FocusedOn(path) =>
      TextFieldEvent.GainedFocus(path).one.pure
    case TextInputOuterEvent.LeftMouseClick(x, y, press) =>
      Monad[UpdateC[TextFieldEvent]].flatMap(
        Update.getCornerCoordinates[TextFieldEvent]
      )(
        cornerCoords =>
          val paragraphHeightShift =
            if textFieldTextAreaSize === minimalClickableAreaSize then
              (textFieldTextAreaSize.height - sizedParagraph.size.height) / 2f
            else
              0f
          val clickPos = Point2d[Float](x, y) - cornerCoords.projectToXY
          if textFieldTextAreaSize.contains(clickPos) then
            val pos = sizedParagraph.value.getGlyphPositionAtCoordinate(
              min(sizedParagraph.size.width, clickPos.x),
              min(sizedParagraph.size.height, clickPos.y - paragraphHeightShift)
            )
            if press then
              requestFocus(currentPath).as(
                List(
                  TextFieldEvent.GainedFocus(currentPath),
                  TextFieldEvent.MoveWholeCursorTo(pos.getPosition)
                )
              )
            else
              TextFieldEvent.MoveCursorTo(pos.getPosition).one.pure
          else
            Nil.pure
          end if
      )
  }

  eventCatcher[TextFieldEvent]:
    textFieldTextArea =>
      Update.currentPath.flatMap(path =>
        Update.handleEnvironmentalEvents_ {
          case DownEvent.UserEvent(event: TextInputOuterEvent) =>
            convertFocusedEvent(textFieldTextArea.size, path)(event).flatMap(emitEvent).as(true)
          case _ => false.pure[UpdateC[TextFieldEvent]]
        }
      )
end textFieldEventCatcher
