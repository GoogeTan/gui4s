package gui4s.desktop.example.shared

import cats.Id
import gui4s.core.geometry.Rect
import gui4s.core.layout.Sized
import gui4s.core.widget.library.textfield.TextFieldEvent
import gui4s.desktop.kit.effects.{LinearContainerPlacementStrategy, Update}
import gui4s.desktop.kit.widgets
import gui4s.desktop.kit.widgets.*
import gui4s.desktop.kit.widgets.decorator.minSizeWidget
import gui4s.desktop.skija.paragraph.Paragraph
import io.github.humbleui.skija.Paint
import io.github.humbleui.skija.paragraph.{FontCollection, ParagraphStyle, TextStyle}

def textField[Event](
  systemEventCatcher : Sized[Rect[Float], Paragraph] => DesktopWidget[TextFieldEvent] => DesktopWidget[TextFieldEvent],
  clipboardSet : String => Update[Event, Unit]
)(
  paragraphStyle: ParagraphStyle,
  fontCollection: FontCollection,
  textStyle: TextStyle,
  selectedTextStyle: TextStyle,
  cursorPaint: Paint = new Paint().setColor(0xFF000000).setStrokeWidth(3f),
  minimalClickableAreaSize: Rect[Float]
)(
  name: String,
  text: String
): DesktopWidget[Event] =
  widgets.textField[Event](
    basicTextFieldBody(
      placeText = textFieldTextPlacement(
        style = paragraphStyle,
        fontCollection = fontCollection,
        textStyle = textStyle,
        selectionStyle = selectedTextStyle,
      ),
      systemEventCatcher = systemEventCatcher,
      drawText = (path, state, text) =>
        minSizeWidget[TextFieldEvent](
          minSize = minimalClickableAreaSize,
          placeHorizontallyIfSmaller = gui4s.core.layout.OneElementPlacementStrategy.Begin,
          placeVerticallyIfSmaller = gui4s.core.layout.OneElementPlacementStrategy.Center,
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
    clipboardSet
  )(name, text)
end textField