package gui4s.desktop.kit.common
package widgets

import effects.*
import effects.Update.given
import widgets.*

import catnip.*
import catnip.syntax.all.given
import cats.*
import cats.effect.kernel.Async
import gui4s.core.geometry.*
import gui4s.core.layout.Sized
import gui4s.core.layout.rowcolumn.{OneElementPlacementStrategy, PlacementStrategy}
import gui4s.desktop.skija.*
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
  sizeText : SizeText[PlaceC[IO]],
  textStyle : TextStyle,
  selectionStyle : TextStyle,
  systemEventCatcher : DesktopWidget[IO, Event] => DesktopWidget[IO, Event],
) : TextFieldState => DesktopWidget[IO, Event] =
  gui4s.desktop.widget.library.basicTextFieldBody[
    DesktopWidget[IO, Event],
    Sized[Float, Paragraph]
  ](
    (state, callback) => ???, //Monad[OuterPlaceT[IO]].flatMap(sizeText(state, textStyle))(callback),
    systemEventCatcher,
    placedParagraph,
  )
end basicTextFieldBody
