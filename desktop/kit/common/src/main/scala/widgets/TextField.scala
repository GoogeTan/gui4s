package gui4s.desktop.kit.common
package widgets

import effects.Update
import effects.Update.given

import cats.MonadThrow
import gui4s.core.layout.Sized
import gui4s.desktop.widget.library.{TextFieldEvent, TextFieldState}
import io.github.humbleui.skija.TextBlob

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
  IO[_] : MonadThrow,
  Event
](
  textPlacer : (text : String, body : Sized[Float, TextBlob] => DesktopWidget[IO, Event]) => DesktopWidget[IO, Event],
  systemEventCatcher : DesktopWidget[IO, Event] => DesktopWidget[IO, Event],
  zIndexContainer : List[DesktopWidget[IO, Event]] => DesktopWidget[IO, Event],
  text : Sized[Float, TextBlob] => DesktopWidget[IO, Event],
  selection : (text : Sized[Float, TextBlob], selectionStart : Int, selectionEndPosition : Int) => DesktopWidget[IO, Event]
) : TextFieldState =>  DesktopWidget[IO, Event] =
  gui4s.desktop.widget.library.basicTextFieldBody(
    textPlacer,
    systemEventCatcher,
    zIndexContainer,
    text,
    selection
  )
end basicTextFieldBody