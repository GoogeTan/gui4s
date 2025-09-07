package gui4s.desktop.widget.library

import catnip.BiMonad
import cats.*
import cats.data.*
import cats.syntax.all.*
import gui4s.core.geometry.Rect

def replaceAt(str : String, position : Int, length : Int, newSubstring : String) : String =
  str.substring(0, position) + newSubstring + str.substring(position + length)
end replaceAt

final case class TextFieldState(text : String, cursorPosition : Int, selectionEndPosition : Int):
  def insertChar(char : Char) : TextFieldState =
    TextFieldState(
      text = replaceAt(text, cursorPosition, cursorPosition - selectionEndPosition, char.toString),
      cursorPosition = cursorPosition + 1,
      selectionEndPosition = cursorPosition + 1
    )
  end insertChar

  def moveCursorTo(position : Int) : TextFieldState =
    TextFieldState(
      text, position, position
    )
  end moveCursorTo

  def moveSeletionTo(position : Int) : TextFieldState =
    if position < cursorPosition then
      copy(cursorPosition = position, selectionEndPosition = cursorPosition)
    else
      copy(selectionEndPosition = position)
end TextFieldState

enum TextFieldEvent:
  case CharInput(char : Char)
  case LeftPressedAt(position : Int)
  case LeftReleasedAt(position : Int)
end TextFieldEvent

def textField[
  Widget[E],
  Update[E, V] : BiMonad as UBM,
  Event
](
  stateful : StatefulWidget[Widget, Update, Nothing],
  body : TextFieldState => Widget[TextFieldEvent]
)(
  name : String,
  text : String,
  onChange : String => Event
) : Widget[Event] =
  given Monad[Update[Event, *]] = UBM()
  stateful[TextFieldState, Event, TextFieldEvent](
    name = name,
    initialState = TextFieldState(text, 0, 0),
    eventHandler = {
      case (state : TextFieldState, _, events : NonEmptyList[TextFieldEvent]) =>
        events.foldLeft(state) {
          case (currentState, TextFieldEvent.CharInput(newChar)) =>
            state.insertChar(newChar)
          case (currentState, TextFieldEvent.LeftPressedAt(position)) =>
            currentState.moveCursorTo(position)
          case (state, TextFieldEvent.LeftReleasedAt(position)) =>
            state.moveSeletionTo(position)
        }.pure[Update[Event, *]]
    },
    body = body
  )
end textField

def basicTextFieldBody[
  Widget,
  PlacedText
](
  textPlacer : (text : String, body : PlacedText => Widget) => Widget,
  systemEventCatcher : Widget => Widget,
  zIndexContainer : List[Widget] => Widget,
  text : PlacedText => Widget,
  selection : (text : PlacedText, selectionStart : Int, selectionEndPosition : Int) => Widget
)(
  state : TextFieldState
) : Widget =
  textPlacer(
    state.text,
    placedText =>
      systemEventCatcher(
        zIndexContainer(
          List(
            text(placedText),
            selection(placedText, state.cursorPosition, state.selectionEndPosition)
          )
        )
      )
  )
end basicTextFieldBody
