package gui4s.desktop.kit.common
package widgets

import effects.*
import effects.Update.given
import widgets.*

import catnip.*
import catnip.syntax.all.{*, given}
import cats.*
import cats.effect.*
import cats.syntax.all.*
import gui4s.core.geometry.*
import gui4s.core.layout.Sized
import gui4s.core.layout.rowcolumn.{OneElementPlacementStrategy, PlacementStrategy}
import gui4s.desktop.skija.*
import gui4s.desktop.skija.paragraph.*
import gui4s.desktop.widget.library.{TextFieldEvent, TextFieldState, TextRange}
import io.github.humbleui.skija.paragraph.*

def textField[
  IO[_] : MonadThrow,
  Event
](
  body : TextFieldState => DesktopWidget[IO, TextFieldEvent]
)(
  name : String,
  text : String,
  onChange : String => Event
) : DesktopWidget[IO, Event] =
  gui4s.desktop.widget.library.textField(
    statefulWidget[IO],
    body
  )(name, text, onChange)
end textField

def basicTextFieldBody[
  IO[_] : Async,
  Event
](
    placeText : TextFieldState => Place[IO, Paragraph],
    systemEventCatcher : DesktopWidget[IO, Event] => DesktopWidget[IO, Event],
) : TextFieldState => DesktopWidget[IO, Event] =
  gui4s.desktop.widget.library.basicTextFieldBody[
    DesktopWidget[IO, Event],
    Sized[Float, Paragraph]
  ](
    (state, callback) => Monad[OuterPlaceT[IO]].flatMap(placeText(state))(callback),
    systemEventCatcher,
    placedParagraph,
  )
end basicTextFieldBody


def textFieldTextPlacement[
  IO[_] : Sync,
  Event
](
  style: ParagraphStyle,
  fontCollection: FontCollection,
  textStyle : TextStyle,
  selectionStyle : TextStyle
)(
    state : TextFieldState
) : OuterPlace[IO, (Sized[Float, Paragraph], IO[Unit])] =
  OuterPlace.liftFunction(
    bounds =>
      textFieldParagraph(
        style,
        fontCollection,
        textStyle,
        selectionStyle,
        state,
      )
      .evalMap(sizeParagraph(_, bounds.width.value))
      .allocated
  )
end textFieldTextPlacement

def textFieldParagraph[IO[_] : Sync](
    style: ParagraphStyle,
    fontCollection: FontCollection,
    textStyle : TextStyle,
    selectionStyle : TextStyle,
    state : TextFieldState,
) : Resource[IO, Paragraph] =
  buildParagraph(
    List(
      (state.textBeforeSelection, textStyle),
      (state.selectedText, selectionStyle),
      (state.textAfterSelection, textStyle)
    ),
    style,
    fontCollection
  )
end textFieldParagraph

def sizeParagraph[IO[_] : Sync](paragraph : Paragraph, availablePlace : Option[Float]) : IO[Sized[Float, Paragraph]] =
  availablePlace.fold(().pure[IO])(layout(_)(paragraph))
    *> size(paragraph).map(size => Sized(paragraph, size))
end sizeParagraph
