package gui4s.desktop.example

import catnip.syntax.all.{*, given}
import cats.*
import cats.effect.*
import cats.effect.std.*
import cats.syntax.all.*
import glfw4s.core.pure.*
import glfw4s.core.{GlfwConstants, KeyAction, WindowCreationSettings}
import glfw4s.jna.bindings.types.{GLFWcursor, GLFWmonitor, GLFWwindow}
import gui4s.core.geometry.{Point2d, Rect}
import gui4s.core.kit.ContainerPlacementError
import gui4s.core.layout.Sized
import gui4s.core.widget.Path
import gui4s.desktop.kit.effects.*
import gui4s.desktop.kit.effects.DownEvent.UserEvent
import gui4s.desktop.kit.{UIApp, widgets}
import gui4s.desktop.kit.widgets.*
import gui4s.desktop.kit.widgets.decorator.*
import gui4s.core.widget.library.TextFieldEvent
import gui4s.core.widget.library.decorator.{Decorator, Paddings}
import io.github.humbleui.skija.paragraph.{FontCollection, Paragraph, ParagraphStyle, TextStyle}
import io.github.humbleui.skija.{FontMgr, Paint}

import scala.math.*

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
                     glfw: PurePostInit[AppIO, IO[Unit], GLFWmonitor, GLFWwindow, GLFWcursor, Int],
                     window: GLFWwindow,
                     eventBus: Queue[IO, DownEvent]
                   ): Resource[AppIO, DesktopWidget[AppIO, Nothing]] =
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
    val stateful = statefulWidget[AppIO]
    val beginMany: LinearContainerPlacementStrategy[AppIO, List] =
      LinearContainerPlacementStrategy.Begin[AppIO, List](5f)
    val begin : OneElementLinearContainerPlacementStrategy[AppIO] =
      LinearContainerPlacementStrategy.Begin[AppIO, Id](0f)
    for
      _ <- textFieldEventSource(glfw, window, eventBus).eval
      textFieldWidget = textField[String](
        glfw,
        window,
        path =>
          Update.liftK[AppIO, TextFieldEvent](
            liftCallbackIOToAppIO(
              eventBus.offer(DownEvent.UserEvent(TextInputOuterEvent.FocusedOn(path)))
            )
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
        columnWidget[AppIO, Nothing](
          (0 until 2).toList.map(i =>
              stateful[String, Nothing, String](
                name = "basic-state-" + i.toString,
                initialState = "A",
                eventHandler = (_, _, newString) => newString.last.pure[UpdateC[AppIO, Nothing]],
                body =
                  state =>
                    gapPaddingWidget[AppIO, String](Paddings(12f, 12f, 12f, 12f))(using Sync[AppIO])(
                      textFieldWidget("text-field", state)
                    )
              )
        ),
          beginMany,
          begin
        )
  end main

  def textField[Event](
                        glfw : PurePostInit[AppIO, IO[Unit], GLFWmonitor, GLFWwindow, GLFWcursor, Int],
                        window : GLFWwindow,
                        requestFocus : Path => Update[AppIO, TextFieldEvent, Unit]
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
                      ) : DesktopWidget[AppIO, Event] =
    widgets.textField[AppIO, Event](
      basicTextFieldBody(
        textFieldTextPlacement(
          style = paragraphStyle,
          fontCollection = fontCollection,
          textStyle = textStyle,
          selectionStyle = selectedTextStyle,
        ),
        textFieldEventCatcher(glfw, window, minimalClickableAreaSize, requestFocus),
        (path, state, text) =>
          minSizeWidget[AppIO, TextFieldEvent](
            minSize = minimalClickableAreaSize,
            placeHorizontally = LinearContainerPlacementStrategy.Begin[AppIO, Id](0f),
            placeVertically = LinearContainerPlacementStrategy.Center[AppIO, Id](0f, ContainerPlacementError.English),
          )(
            stackContainer(
              List(
                placedParagraph[AppIO, TextFieldEvent](text),
              ) ++ Option.when(state.isFocused(path))(
                drawCursor[AppIO, TextFieldEvent](text, state.cursorStartIndex, cursorPaint)
              ),
              LinearContainerPlacementStrategy.Begin[AppIO, Id](0),
              LinearContainerPlacementStrategy.Begin[AppIO, Id](0)
            )
          ),
      ),
      text =>
        Update.liftK[AppIO, Event](
          glfw.setClipboardString(window, text)
        )
  )(name, text, update)
  end textField

  def textFieldEventSource(
                            glfw: PurePostInit[AppIO, IO[Unit], GLFWmonitor, GLFWwindow, GLFWcursor, Int],
                            window: GLFWwindow,
                            eventBus: Queue[IO, DownEvent]
                          ): AppIO[Unit] =
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
          (
            for
              monitor <- glfw.getPrimaryMonitor
              (scaleX, scaleY) <- glfw.getMonitorContentScale(monitor.get)
              (x, y) <- glfw.getCursorPos(window)
            yield ((x * scaleX).toFloat, (y * scaleY).toFloat)
          ).value.flatMap {
            case Right((x, y)) =>
              eventBus.offer(DownEvent.UserEvent(TextInputOuterEvent.LeftMouseClick(x, y, action == KeyAction.Press)))
            case Left(error) =>
              IO.raiseError(new Exception(error.toString))
          }
        else
          IO.unit
        end if
      )
  end textFieldEventSource

  def textFieldEventCatcher(
                             glfw: PurePostInit[AppIO, IO[Unit], GLFWmonitor, GLFWwindow, GLFWcursor, Int],
                             window: GLFWwindow,
                             minimalClickableAreaSize : Rect[Float],
                             requestFocus : Path => Update[AppIO, TextFieldEvent, Unit]
                           )(
                             sizedParagraph : Sized[Float, Paragraph],
                           ) : Decorator[DesktopWidget[AppIO, TextFieldEvent]] =
    val emitEvent : List[TextFieldEvent] => Update[AppIO, TextFieldEvent, Boolean] = events =>
      Update.emitEvents[AppIO, TextFieldEvent](events).as(true)

    def convertFocusedEvent(textFieldTextAreaSize : Rect[Float], currentPath : Path) : TextInputOuterEvent => Update[AppIO, TextFieldEvent, List[TextFieldEvent]] = {
      case TextInputOuterEvent.CharInputEvent(key) => TextFieldEvent.CharInput(key.toChar).one.pure
      case TextInputOuterEvent.Backspace => TextFieldEvent.Backspace.one.pure
      case TextInputOuterEvent.Enter => TextFieldEvent.CharInput('\n').one.pure
      case TextInputOuterEvent.Left(shift) => TextFieldEvent.GoLeft(shift).one.pure
      case TextInputOuterEvent.Right(shift) => TextFieldEvent.GoRight(shift).one.pure
      case TextInputOuterEvent.Up(shift) => TextFieldEvent.GoUp(shift).one.pure
      case TextInputOuterEvent.Down(shift) => TextFieldEvent.GoDown(shift).one.pure
      case TextInputOuterEvent.ClipbordPaste =>
        Update.liftK[AppIO, TextFieldEvent](
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
        Monad[UpdateC[AppIO, TextFieldEvent]].flatMap(
            Update.getCornerCoordinates[AppIO, TextFieldEvent]
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

    eventCatcher[AppIO, TextFieldEvent] {
      case (path, textFieldTextArea, DownEvent.UserEvent(event : TextInputOuterEvent)) =>
        convertFocusedEvent(textFieldTextArea.size, path)(event).flatMap(emitEvent)
      case _ => false.pure[UpdateC[AppIO, TextFieldEvent]]
    }
  end textFieldEventCatcher
end TextInputExample