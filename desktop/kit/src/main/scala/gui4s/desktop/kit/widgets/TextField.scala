package gui4s.desktop.kit
package widgets

import cats.*
import cats.effect.*
import cats.syntax.all.*
import gui4s.core.layout.Sized
import gui4s.core.widget.Path
import gui4s.desktop.kit.effects.Update.given
import gui4s.desktop.kit.effects.*
import gui4s.desktop.kit.widgets.*
import gui4s.desktop.skija.paragraph.*
import gui4s.core.widget.library.{TextFieldEvent, TextFieldState}
import io.github.humbleui.skija.Paint
import io.github.humbleui.skija.paragraph.*

def textField[
  IO[_] : MonadThrow,
  Event
](
  body : TextFieldState => DesktopWidget[IO, TextFieldEvent],
  copyTextToClipboard : String => Update[IO, Event, Unit],
)(
  name : String,
  text : String,
  onChange : String => Event
) : DesktopWidget[IO, Event] =
  gui4s.core.widget.library.textField(
    statefulWidget[IO],
    body,
    copyTextToClipboard
  )(name, text, onChange)
end textField

def basicTextFieldBody[
  IO[_] : Async,
  Event
](
    placeText : TextFieldState => Place[IO, Paragraph],
    systemEventCatcher : Sized[Float, Paragraph] => DesktopWidget[IO, Event] => DesktopWidget[IO, Event],
    drawText : (Path, TextFieldState, Sized[Float, Paragraph]) => DesktopWidget[IO, Event]
) : TextFieldState => DesktopWidget[IO, Event] =
  state =>
    gui4s.core.widget.library.basicTextFieldBody[
      DesktopWidget[IO, Event],
      Sized[Float, Paragraph]
    ](
      (state, callback) => Monad[OuterPlaceC[IO]].flatMap(placeText(state))(callback),
      systemEventCatcher,
      text =>
        OuterPlace.currentPath[IO].flatMap(path => drawText(path, state, text))
    )(state)
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
) : Place[IO, Paragraph] =
  OuterPlace.liftFunction(
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

def textFieldParagraph[IO[_] : Sync](
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

def sizeParagraph[IO[_] : Sync](paragraph : Paragraph, availablePlace : Option[Float]) : IO[Sized[Float, Paragraph]] =
  availablePlace.fold(().pure[IO])(layout(_)(paragraph))
    *> size(paragraph).map(size => Sized(paragraph, size))
end sizeParagraph
