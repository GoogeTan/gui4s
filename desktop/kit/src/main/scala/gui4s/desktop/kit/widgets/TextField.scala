package gui4s.desktop.kit
package widgets

import cats._
import cats.effect._
import cats.syntax.all._

import gui4s.core.geometry.Rect
import gui4s.core.layout.Sized
import gui4s.core.widget.Path
import gui4s.core.widget.library.textfield.TextFieldEvent
import gui4s.core.widget.library.textfield.TextFieldState

import gui4s.desktop.kit.effects.Update.given
import gui4s.desktop.kit.effects._
import gui4s.desktop.kit.widgets._
import gui4s.desktop.skija.paragraph._

def textField[
  Event
](
  body : TextFieldState => DesktopWidget[TextFieldEvent],
  copyTextToClipboard : String => Update[Event, Unit],
)(
  name : String,
  text : String,
  onChange : String => Event
) : DesktopWidget[Event] =
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
    systemEventCatcher : Sized[Rect[Float], Paragraph] => DesktopWidget[Event] => DesktopWidget[Event],
    drawText : (Path, TextFieldState, Sized[Rect[Float], Paragraph]) => DesktopWidget[Event]
) : TextFieldState => DesktopWidget[Event] =
  state =>
    gui4s.core.widget.library.textfield.basicTextFieldBody[
      DesktopWidget[Event],
      Sized[Rect[Float], Paragraph]
    ](
      (state, callback) => Monad[PlacementEffect].flatMap(placeText(state))(callback),
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
  availablePlace.fold(
    ().pure[IO]
  )(
    layout[IO](_)(paragraph)
  ) *> size[IO](paragraph).map(size => Sized(paragraph, size))
end sizeParagraph
