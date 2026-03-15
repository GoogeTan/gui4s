package gui4s.android.kit
package widgets

import cats.*
import cats.effect.*
import cats.syntax.all.*
import gui4s.core.layout.Sized
import gui4s.core.widget.Path
import gui4s.android.kit.effects.Update.given
import gui4s.android.kit.effects.*
import gui4s.android.kit.widgets.*
import gui4s.android.skia.paragraph.*
import gui4s.core.widget.library.textfield.{TextFieldEvent, TextFieldState}
import org.jetbrains.skia.paragraph.*
import cats.effect.IO
import cats.*
import cats.effect.*
import cats.syntax.all.*
import gui4s.core.layout.Sized
import gui4s.core.widget.Path
import gui4s.android.kit.effects.Update.given
import gui4s.android.kit.effects.*
import gui4s.android.kit.widgets.*
import gui4s.android.skia.paragraph.*
import gui4s.core.geometry.Rect
import gui4s.core.widget.library.textfield.{TextFieldEvent, TextFieldState}
import org.jetbrains.skia.paragraph.*

def textField[
  Event
](
  body : TextFieldState => AndroidWidget[TextFieldEvent],
  copyTextToClipboard : String => Update[Event, Unit],
)(
  name : String,
  text : String,
  onChange : String => Event
) : AndroidWidget[Event] =
  gui4s.core.widget.library.textfield.textField(
    statefulWidget,
    body,
    copyTextToClipboard
  )(name, text, onChange)
end textField

def basicTextFieldBody[
  Event
](
  placeText : TextFieldState => Place[Paragraph],
  systemEventCatcher : Sized[Rect[Float], Paragraph] => AndroidWidget[Event] => AndroidWidget[Event],
  drawText : (Path, TextFieldState, Sized[Rect[Float], Paragraph]) => AndroidWidget[Event]
) : TextFieldState => AndroidWidget[Event] =
  state =>
    gui4s.core.widget.library.textfield.basicTextFieldBody[
      AndroidWidget[Event],
      Sized[Rect[Float], Paragraph]
    ](
      (state, callback) => Monad[PlacementEffectC].flatMap(placeText(state))(callback),
      systemEventCatcher,
      text =>
        PlacementEffect.currentPath.flatMap(path => drawText(path, state, text))
    )(state)
end basicTextFieldBody


def textFieldTextPlacement[
  Event
](
  style: ParagraphStyle,
  fontCollection: FontCollection,
  textStyle : TextStyle,
  selectionStyle : TextStyle
)(
  state : TextFieldState
) : Place[Paragraph] =
  PlacementEffect.liftFunction(
    bounds =>
      textFieldParagraph(
        style,
        fontCollection,
        textStyle,
        selectionStyle,
        state,
      )
        .flatMap(sizeParagraph(_, bounds.width.value))
  )
end textFieldTextPlacement

def textFieldParagraph(
                                      style: ParagraphStyle,
                                      fontCollection: FontCollection,
                                      textStyle : TextStyle,
                                      selectionStyle : TextStyle,
                                      state : TextFieldState,
                                    ) : IO[Paragraph] =
  buildParagraph(
    List(
      (state.getBefore, textStyle),
      (state.getSelected, selectionStyle),
      (state.getAfter, textStyle)
    ),
    style,
    fontCollection
  )
end textFieldParagraph

def sizeParagraph(paragraph : Paragraph, availablePlace : Option[Float]) : IO[Sized[Rect[Float], Paragraph]] =
  availablePlace.fold(().pure[IO])(layout[IO](_)(paragraph))
    *> size[IO](paragraph).map(size => Sized(paragraph, size))
end sizeParagraph
