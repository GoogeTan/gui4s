package gui4s.desktop.example

import scala.math.*
import catnip.syntax.all.{*, given}
import cats.*
import cats.effect.*
import cats.effect.std.*
import cats.syntax.all.*
import glfw4s.core.GlfwConstants
import glfw4s.core.KeyAction
import glfw4s.core.WindowCreationSettings
import glfw4s.core.pure.*
import glfw4s.jna.bindings.types.GLFWcursor
import glfw4s.jna.bindings.types.GLFWmonitor
import glfw4s.jna.bindings.types.GLFWwindow
import io.github.humbleui.skija.FontMgr
import io.github.humbleui.skija.Paint
import io.github.humbleui.skija.paragraph.FontCollection
import io.github.humbleui.skija.paragraph.Paragraph
import io.github.humbleui.skija.paragraph.ParagraphStyle
import io.github.humbleui.skija.paragraph.TextStyle
import gui4s.core.geometry.Point2d
import gui4s.core.geometry.Rect
import gui4s.core.kit.ContainerPlacementError
import gui4s.core.layout.Sized
import gui4s.core.widget.Path
import gui4s.core.widget.library.decorator.Decorator
import gui4s.core.widget.library.decorator.Paddings
import gui4s.core.widget.library.textfield.TextFieldEvent
import gui4s.desktop.kit.UIApp
import gui4s.desktop.kit.effects.DownEvent.UserEvent
import gui4s.desktop.kit.effects.*
import gui4s.desktop.kit.widgets
import gui4s.desktop.kit.widgets.*
import gui4s.desktop.kit.widgets.decorator.*
import gui4s.desktop.skija.{Brush, StrokeOptions}

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
  override val settings: WindowCreationSettings[GLFWmonitor, GLFWwindow] = WindowCreationSettings(
      title = "Text input example",
      width = 640,
      height = 480
  )

  override def main(
                     glfw: PurePostInit[IO, IO[Unit], GLFWmonitor, GLFWwindow, GLFWcursor, Int],
                     window: GLFWwindow,
                     eventBus: Queue[IO, DownEvent]
                   ): Resource[IO, DesktopWidget[Nothing]] =
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
      _ <- textFieldEventSource(glfw, window, eventBus).eval
      textFieldWidget = textField[String](
        glfw,
        window,
        path =>
          Update.liftK[IO, TextFieldEvent](
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
                eventHandler = (_, _, newString) => newString.last.pure[UpdateC[IO, Nothing]],
                body =
                  state =>
                    textFieldWidget("text-field", state)
                      .padding(Paddings(6f, 6f, 6f, 6f))
                      .border(
                        Shapes.roundedCorners(50f),
                        Brush.solid(0xFF454649),
                        StrokeOptions()
                      )
                      .padding(Paddings(12f, 12f, 12f, 12f))
              )
          )
        )
  end main

  def textField[Event](
                        glfw : PurePostInit[IO, IO[Unit], GLFWmonitor, GLFWwindow, GLFWcursor, Int],
                        window : GLFWwindow,
                        requestFocus : Path => Update[IO, TextFieldEvent, Unit]
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
        textFieldEventCatcher(glfw, window, minimalClickableAreaSize, requestFocus),
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
        Update.liftK[IO, Event](
          glfw.setClipboardString(window, text)
        )
  )(name, text, update)
  end textField

  def textFieldEventSource(
                            glfw: PurePostInit[IO, IO[Unit], GLFWmonitor, GLFWwindow, GLFWcursor, Int],
                            window: GLFWwindow,
                            eventBus: Queue[IO, DownEvent]
                          ): IO[Unit] =
    glfw.addCharCallback(window, (window, key) =>
      eventBus.offer(DownEvent.UserEvent(TextInputOuterEvent.CharInputEvent(key)))
    ) *>
      glfw.addKeyCallback(window, (window, key, scancode, action, mods) =>
        if action != KeyAction.Press then
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
      ) *> glfw.addMouseButtonCallback(window, (window, button, action, mods) =>
        if button == GlfwConstants.GLFW_MOUSE_BUTTON_LEFT && action != KeyAction.Repeat then
            for
              monitor <- glfw.getPrimaryMonitor
              (scaleX, scaleY) <- glfw.getMonitorContentScale(monitor.get)
              (x, y) <- glfw.getCursorPos(window)
              _ <- eventBus.offer(
                DownEvent.UserEvent(
                  TextInputOuterEvent.LeftMouseClick((x * scaleX).toFloat, (y * scaleY).toFloat, action == KeyAction.Press)
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
                             minimalClickableAreaSize : Rect[Float],
                             requestFocus : Path => Update[IO, TextFieldEvent, Unit]
                           )(
                             sizedParagraph : Sized[Float, Paragraph],
                           ) : Decorator[DesktopWidget[TextFieldEvent]] =
    val emitEvent : List[TextFieldEvent] => Update[IO, TextFieldEvent, Boolean] = events =>
      Update.emitEvents[IO, TextFieldEvent](events).as(true)

    def convertFocusedEvent(textFieldTextAreaSize : Rect[Float], currentPath : Path) : TextInputOuterEvent => Update[IO, TextFieldEvent, List[TextFieldEvent]] = {
      case TextInputOuterEvent.CharInputEvent(key) => TextFieldEvent.CharInput(key.toChar).one.pure
      case TextInputOuterEvent.Backspace => TextFieldEvent.Backspace.one.pure
      case TextInputOuterEvent.Enter => TextFieldEvent.CharInput('\n').one.pure
      case TextInputOuterEvent.Left(shift) => TextFieldEvent.GoLeft(shift).one.pure
      case TextInputOuterEvent.Right(shift) => TextFieldEvent.GoRight(shift).one.pure
      case TextInputOuterEvent.Up(shift) => TextFieldEvent.GoUp(shift).one.pure
      case TextInputOuterEvent.Down(shift) => TextFieldEvent.GoDown(shift).one.pure
      case TextInputOuterEvent.ClipbordPaste =>
        Update.liftK[IO, TextFieldEvent](
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
        Monad[UpdateC[IO, TextFieldEvent]].flatMap(
            Update.getCornerCoordinates[IO, TextFieldEvent]
        )(
          cornerCoords =>
            val paragraphHeightShift =
              if textFieldTextAreaSize == minimalClickableAreaSize then
                (textFieldTextAreaSize.height - sizedParagraph.height) / 2f
              else
                0f
            val clickPos = Point2d[Float](x,y) - cornerCoords.projectToXY
            if textFieldTextAreaSize.contains(clickPos) then
              val pos = sizedParagraph.value.getGlyphPositionAtCoordinate(
                min(sizedParagraph.width, clickPos.x),
                min(sizedParagraph.height, clickPos.y - paragraphHeightShift)
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

    eventCatcher[TextFieldEvent] {
      case (path, textFieldTextArea, DownEvent.UserEvent(event : TextInputOuterEvent)) =>
        convertFocusedEvent(textFieldTextArea.size, path)(event).flatMap(emitEvent)
      case _ => false.pure[UpdateC[IO, TextFieldEvent]]
    }
  end textFieldEventCatcher
end TextInputExample