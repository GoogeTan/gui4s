package gui4s.desktop.example.jwm

import cats.*
import cats.effect.*
import cats.effect.std.*
import cats.syntax.all.*
import gui4s.core.geometry.Rect
import gui4s.core.widget.Path
import gui4s.core.widget.library.decorator.Paddings
import gui4s.core.widget.library.textfield.TextFieldEvent
import gui4s.desktop.kit.effects.*
import gui4s.desktop.kit.widgets
import gui4s.desktop.kit.widgets.*
import gui4s.desktop.kit.widgets.decorator.*
import gui4s.desktop.skija.{Brush, StrokeOptions}
import gui4s.desktop.windowing.jwm.UIApp
import io.github.humbleui.jwm.*
import io.github.humbleui.skija.{FontMgr, Paint}
import io.github.humbleui.skija.paragraph.{FontCollection, ParagraphStyle, TextStyle}
import main.scala.gui4s.desktop.windowing.jwm.{TextInputOuterEvent, textFieldEventCatcher, textFieldEventSource}

import scala.math.*

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
    for
      _ <- Init.eval(textFieldEventSource(window, eventBus))
      textFieldWidget = textField[String](
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
      )(_ : String, _ : String)
    yield
      columnWidget[Nothing](
        (0 until 2).toList.map(i =>
          simpleStatefulWidget[String, Nothing, String](
            name = "basic-state-" + i.toString,
            initialState = "A",
            eventHandler = (_, newString) =>
              newString.lastOption.pure[UpdateC[Nothing]],
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
    text : String
  ) : DesktopWidget[Event] =
    gui4s.desktop.example.shared.textField(
      systemEventCatcher = textFieldEventCatcher(
        minimalClickableAreaSize,
        requestFocus
      ),
      clipboardSet = text =>
        Update.liftK[Event](
          IO.delay(Clipboard.set(ClipboardEntry.makeString(ClipboardFormat.TEXT, text)))
        )
    )(
      paragraphStyle,
      fontCollection,
      textStyle,
      selectedTextStyle,
      cursorPaint,
      minimalClickableAreaSize
    )(
      name,
      text
    )
  end textField

end TextInputExample