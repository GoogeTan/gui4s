package gui4s.desktop.windowing.jwm

import cats.syntax.all.*
import catnip.syntax.all.*
import cats.Monad
import cats.effect.IO
import cats.effect.std.Queue
import cats.effect.unsafe.IORuntime
import gui4s.core.geometry.{Point2d, Rect}
import gui4s.core.layout.Sized
import gui4s.core.widget.Path
import gui4s.core.widget.library.decorator.Decorator
import gui4s.core.widget.library.textfield.TextFieldEvent
import gui4s.desktop.kit.effects.{DownEvent, Update, UpdateC}
import gui4s.desktop.kit.widgets.DesktopWidget
import gui4s.desktop.kit.widgets.decorator.eventCatcher
import io.github.humbleui.jwm.{Clipboard, ClipboardFormat, EventKey, EventMouseButton, EventTextInput, Key, KeyModifier, MouseButton, Window}
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


@SuppressWarnings(Array("org.wartremover.warts.Equals"))
def textFieldEventSource(
  window: Window,
  eventBus: Queue[IO, DownEvent]
)(using IORuntime): IO[Unit] = IO.delay {
  window.setEventListener {
    case textEvent: EventTextInput =>
      val text = textEvent.getText
      if (text.nonEmpty) {
        eventBus.offer(DownEvent.UserEvent(TextInputOuterEvent.CharInputEvent(text.head.toInt))).unsafeRunAndForget()
      }

    case keyEvent: EventKey =>
      if (keyEvent.isPressed) {
        val shift = keyEvent.isModifierDown(KeyModifier.SHIFT)
        val ctrlOrCmd = keyEvent.isModifierDown(KeyModifier.CONTROL) || keyEvent.isModifierDown(KeyModifier.MAC_COMMAND)

        keyEvent.getKey match {
          case Key.BACKSPACE =>
            eventBus.offer(DownEvent.UserEvent(TextInputOuterEvent.Backspace)).unsafeRunAndForget()
          case Key.ENTER =>
            eventBus.offer(DownEvent.UserEvent(TextInputOuterEvent.Enter)).unsafeRunAndForget()
          case Key.LEFT =>
            eventBus.offer(DownEvent.UserEvent(TextInputOuterEvent.Left(shift))).unsafeRunAndForget()
          case Key.RIGHT =>
            eventBus.offer(DownEvent.UserEvent(TextInputOuterEvent.Right(shift))).unsafeRunAndForget()
          case Key.UP =>
            eventBus.offer(DownEvent.UserEvent(TextInputOuterEvent.Up(shift))).unsafeRunAndForget()
          case Key.DOWN =>
            eventBus.offer(DownEvent.UserEvent(TextInputOuterEvent.Down(shift))).unsafeRunAndForget()
          case Key.V if ctrlOrCmd =>
            eventBus.offer(DownEvent.UserEvent(TextInputOuterEvent.ClipbordPaste)).unsafeRunAndForget()
          case Key.C if ctrlOrCmd =>
            eventBus.offer(DownEvent.UserEvent(TextInputOuterEvent.ClipbordCopy)).unsafeRunAndForget()
          case Key.X if ctrlOrCmd =>
            eventBus.offer(DownEvent.UserEvent(TextInputOuterEvent.ClipbordCut)).unsafeRunAndForget()
          case Key.A if ctrlOrCmd =>
            eventBus.offer(DownEvent.UserEvent(TextInputOuterEvent.SelectAll)).unsafeRunAndForget()
          case _ => ()
        }
      }

    case mouseEvent: EventMouseButton =>
      if mouseEvent.getButton == MouseButton.PRIMARY then 
        val scale = window.getScreen.getScale
        val x = mouseEvent.getX
        val y = mouseEvent.getY
        eventBus.offer(
          DownEvent.UserEvent(
            TextInputOuterEvent.LeftMouseClick((x * scale), (y * scale), mouseEvent.isPressed)
          )
        ).unsafeRunAndForget()
      end if
    case _ => ()
  }
}
end textFieldEventSource

@SuppressWarnings(Array("org.wartremover.warts.Equals"))
def textFieldEventCatcher(
  minimalClickableAreaSize: Rect[Float],
  requestFocus: Path => Update[TextFieldEvent, Unit]
)(
  sizedParagraph: Sized[Rect[Float], Paragraph],
): Decorator[DesktopWidget[TextFieldEvent]] =
  val emitEvent: List[TextFieldEvent] => Update[TextFieldEvent, Boolean] = events =>
    Update.emitEvents[TextFieldEvent](events).as(true)

  @SuppressWarnings(Array("org.wartremover.warts.Null"))
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
        IO.delay {
          val entry = Clipboard.get(ClipboardFormat.TEXT)
          if (entry != null && entry.getString =!= null)
            List(TextFieldEvent.ClipboardPaste(entry.getString))
          else
            Nil
        }
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
            if textFieldTextAreaSize == minimalClickableAreaSize then
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
          case _ =>
            false.pure[UpdateC[TextFieldEvent]]
        }
      )
end textFieldEventCatcher