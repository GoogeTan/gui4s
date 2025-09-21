package gui4s.desktop.widget.library

import catnip.BiMonad
import cats.*
import cats.data.*
import cats.syntax.all.*
import gui4s.core.geometry.Rect
import gui4s.desktop.widget.library.TextRange.replaceAt


final case class TextFieldState(text : String, selection : TextRange):
  def this(text : String) =
    this(text, TextRange(0))
  end this
  
  def insertText(textToInsert : String) : TextFieldState =
    TextFieldState(
      text = text.replaceAt(selection, textToInsert ),
      selection = TextRange(selection.start + 1)
    )
  end insertText

  def moveCursorTo(position : Int) : TextFieldState =
    copy(
      selection = TextRange(position)
    )
  end moveCursorTo

  def moveSeletionTo(position : Int) : TextFieldState =
    copy(
      selection = TextRange(selection.start, position)
    )
  end moveSeletionTo

  def textBeforeSelection : String =
    text.substring(0, selection.start)
  end textBeforeSelection

  def selectedText : String =
    text.substring(selection.start, selection.end)
  end selectedText

  def textAfterSelection : String =
    text.substring(selection.end)
  end textAfterSelection
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
    initialState = new TextFieldState(text),
    eventHandler = {
      case (state : TextFieldState, _, events : NonEmptyList[TextFieldEvent]) =>
        events.foldLeft(state) {
          case (currentState, TextFieldEvent.CharInput(newChar)) =>
            state.insertText(newChar.toString)
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
  textPlacer : (state :TextFieldState, body : PlacedText => Widget) => Widget,
  systemEventCatcher : Widget => Widget,
  text : PlacedText => Widget,
)(
  state : TextFieldState
) : Widget =
  textPlacer(
    state,
    placedText =>
      systemEventCatcher(
            text(placedText),
      )
  )
end basicTextFieldBody
