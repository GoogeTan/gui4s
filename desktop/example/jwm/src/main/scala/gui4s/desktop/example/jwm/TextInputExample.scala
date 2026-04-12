package gui4s.desktop.example.jwm

import scala.math.*
import catnip.syntax.all.*
import cats.*
import cats.effect.*
import cats.effect.std.*
import cats.syntax.all.*
import cats.effect.unsafe.implicits.global // Required to run IO effects in JWM's listener callbacks
import io.github.humbleui.skija.FontMgr
import io.github.humbleui.skija.Paint
import io.github.humbleui.skija.paragraph.FontCollection
import io.github.humbleui.skija.paragraph.Paragraph
import io.github.humbleui.skija.paragraph.ParagraphStyle
import io.github.humbleui.skija.paragraph.TextStyle
import io.github.humbleui.jwm.* // JWM specific imports
import gui4s.core.geometry.Point2d
import gui4s.core.geometry.Rect
import gui4s.core.kit.ContainerPlacementError
import gui4s.core.layout.Sized
import gui4s.core.widget.Path
import gui4s.core.widget.library.decorator.Decorator
import gui4s.core.widget.library.decorator.Paddings
import gui4s.core.widget.library.textfield.TextFieldEvent
import gui4s.desktop.windowing.jwm.UIApp
import gui4s.desktop.kit.effects.DownEvent.UserEvent
import gui4s.desktop.kit.effects.*
import gui4s.desktop.kit.widgets
import gui4s.desktop.kit.widgets.*
import gui4s.desktop.kit.widgets.decorator.*
import gui4s.desktop.skija.Brush
import gui4s.desktop.skija.StrokeOptions

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

object TextInputExample extends UIApp:
  override def main(
    window : Window,
    eventBus: Queue[IO, DownEvent]
  ): Init[DesktopWidget[Nothing]] =
    val fontCollection = new FontCollection
    fontCollection.setDefaultFontManager(FontMgr.getDefault)
    val paint = new Paint().setColor(0xFF454649)
    val textStyle = new TextStyle()
      .setForeground(paint)
      .setFontSize(32)
    val selectedTextStyle = new TextStyle()
      .setBackground(new Paint().setColor(0xFFDCDCDC))
      .setForeground(paint)
      .setFontSize(32)
    val paragraphStyle = new ParagraphStyle().setTextStyle(textStyle)
    val cursorPaint = new Paint().setColor(0xFF000000).setStrokeWidth(3f)
    val stateful = statefulWidget
    val beginMany: LinearContainerPlacementStrategy[List] =
      LinearContainerPlacementStrategy.Begin[List](5f)
    val begin : OneElementLinearContainerPlacementStrategy =
      LinearContainerPlacementStrategy.Begin[Id](0f)

    for
      _ <- Init.eval(textFieldEventSource(window, eventBus)) // GLFW removed
      textFieldWidget = textField[String](
        window,
        path =>
          Update.liftK[TextFieldEvent](
            eventBus.offer(DownEvent.UserEvent(TextInputOuterEvent.FocusedOn(path)))
          )
      )(
        paragraphStyle,
        fontCollection,
        textStyle,
        selectedTextStyle,
        cursorPaint,
        Rect(100, 30)
      )(_ : String, _ : String, identity)
    yield
      columnWidget[Nothing](
        (0 until 2).toList.map(i =>
          stateful[String, Nothing, String](
            name = "basic-state-" + i.toString,
            initialState = "A",
            eventHandler = (_, newString) => newString.last.pure[UpdateC[Nothing]],
            body =
              state =>
                textFieldWidget("text-field", state)
                  .padding(Paddings(6f, 6f, 6f, 6f))
                  .border(
                    Shape.roundedCorners(50f),
                    Brush.solid(0xFF454649),
                    StrokeOptions()
                  )
                  .padding(Paddings(12f, 12f, 12f, 12f))
          )
        )
      )
  end main

  def textField[Event](
    window : Window,
    requestFocus : Path => Update[TextFieldEvent, Unit]
  )(
    paragraphStyle: ParagraphStyle,
    fontCollection: FontCollection,
    textStyle: TextStyle,
    selectedTextStyle: TextStyle,
    cursorPaint: Paint = new Paint().setColor(0xFF000000).setStrokeWidth(3f),
    minimalClickableAreaSize : Rect[Float]
  )(
    name : String,
    text : String,
    update : String => Event
  ) : DesktopWidget[Event] =
    widgets.textField[Event](
      basicTextFieldBody(
        textFieldTextPlacement(
          style = paragraphStyle,
          fontCollection = fontCollection,
          textStyle = textStyle,
          selectionStyle = selectedTextStyle,
        ),
        textFieldEventCatcher(window, minimalClickableAreaSize, requestFocus),
        (path, state, text) =>
          minSizeWidget[TextFieldEvent](
            minSize = minimalClickableAreaSize,
            placeHorizontally = LinearContainerPlacementStrategy.Begin[Id](0f),
            placeVertically = LinearContainerPlacementStrategy.Center[Id](0f, ContainerPlacementError.English),
          )(
            stackContainer(
              List(
                placedParagraph[TextFieldEvent](text),
              ) ++ Option.when(state.isFocused(path))(
                drawCursor[TextFieldEvent](text, state.cursorStartIndex, cursorPaint)
              ),
              LinearContainerPlacementStrategy.Begin[Id](0),
              LinearContainerPlacementStrategy.Begin[Id](0)
            )
          ),
      ),
      text =>
        Update.liftK[Event](
          IO.delay(Clipboard.set(ClipboardEntry.makeString(ClipboardFormat.TEXT, text)))
        )
    )(name, text, update)
  end textField

  def textFieldEventSource(
    window: Window,
    eventBus: Queue[IO, DownEvent]
  ): IO[Unit] = IO.delay {
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
        if (mouseEvent.getButton == MouseButton.PRIMARY) {
          val scale = window.getScreen.getScale
          val x = mouseEvent.getX
          val y = mouseEvent.getY
          eventBus.offer(
            DownEvent.UserEvent(
              TextInputOuterEvent.LeftMouseClick((x * scale).toFloat, (y * scale).toFloat, mouseEvent.isPressed)
            )
          ).unsafeRunAndForget()
        }

      case _ => ()
    }
  }
  end textFieldEventSource

  def textFieldEventCatcher(
    window: Window,
    minimalClickableAreaSize : Rect[Float],
    requestFocus : Path => Update[TextFieldEvent, Unit]
  )(
    sizedParagraph : Sized[Rect[Float], Paragraph],
  ) : Decorator[DesktopWidget[TextFieldEvent]] =
    val emitEvent : List[TextFieldEvent] => Update[TextFieldEvent, Boolean] = events =>
      Update.emitEvents[TextFieldEvent](events).as(true)

    def convertFocusedEvent(textFieldTextAreaSize : Rect[Float], currentPath : Path) : TextInputOuterEvent => Update[TextFieldEvent, List[TextFieldEvent]] = {
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
            if (entry != null && entry.getString != null)
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
            val clickPos = Point2d[Float](x,y) - cornerCoords.projectToXY
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
            case DownEvent.UserEvent(event : TextInputOuterEvent) =>
              convertFocusedEvent(textFieldTextArea.size, path)(event).flatMap(emitEvent).as(true)
            case _ => false.pure[UpdateC[TextFieldEvent]]
          }
        )

  end textFieldEventCatcher
end TextInputExample