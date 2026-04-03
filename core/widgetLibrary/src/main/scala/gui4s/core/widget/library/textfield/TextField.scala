package gui4s.core.widget.library.textfield

import catnip.BiMonad
import cats._
import cats.data._
import cats.syntax.all._

import gui4s.core.widget.library.StatefulWidget

def textField[
  Widget[E],
  Update[E, V] : BiMonad as UBM,
  Event
](
  stateful : StatefulWidget[Widget, Update, Nothing, Nothing],
  body : TextFieldState => Widget[TextFieldEvent],
  copyTextToClipboard : String => Update[Event, Unit],
)(
   name : String,
   text : String,
   onChange : String => Event
 ) : Widget[Event] =
  given Monad[Update[Event, *]] = UBM()
  stateful[TextFieldState, Event, TextFieldEvent](
    name = name,
    initialState = TextFieldState(text.split("\n").toList, TextPosition(0, 0), TextPosition(0, 0)),
    eventHandler = { (stateDontTouch : TextFieldState, path, events : List[TextFieldEvent]) =>
      events.reverse.foldM(stateDontTouch) {
        case (currentState, TextFieldEvent.CharInput(newChar)) if currentState.isFocused(path) =>
          currentState.insert(newChar.toString).pure
        case (currentState, TextFieldEvent.Backspace) if currentState.isFocused(path)  =>
          currentState.backspace().pure
        case (currentState, TextFieldEvent.GoLeft(shift)) if currentState.isFocused(path)  =>
          currentState.moveCursorLeft(shift).pure
        case (currentState, TextFieldEvent.GoRight(shift)) if currentState.isFocused(path)  =>
          currentState.moveCursorRight(shift).pure
        case (currentState, TextFieldEvent.GoUp(shift)) if currentState.isFocused(path)  =>
          currentState.moveCursorUp(shift).pure
        case (currentState, TextFieldEvent.GoDown(shift)) if currentState.isFocused(path)  =>
          currentState.moveCursorDown(shift).pure
        case (currentState, TextFieldEvent.MoveWholeCursorTo(pos)) if currentState.isFocused(path)  =>
          currentState.putWholeCursorTo(currentState.textPositionOf(pos)).pure
        case (currentState, TextFieldEvent.MoveCursorTo(pos)) if currentState.isFocused(path)  =>
          currentState.putCursorTo(currentState.textPositionOf(pos)).pure
        case (currentState, TextFieldEvent.ClipboardPaste(text)) if currentState.isFocused(path)  =>
          currentState.insert(text).pure
        case (currentState, TextFieldEvent.ClipboardCopy) if currentState.isFocused(path)  =>
          if currentState.getSelected.nonEmpty then
            copyTextToClipboard(currentState.getSelected).as(currentState)
          else
            currentState.pure
        case (currentState, TextFieldEvent.ClipboardCut) if currentState.isFocused(path)  =>
          if currentState.getSelected.nonEmpty then
            copyTextToClipboard(currentState.getSelected).as(currentState.backspace())
          else
            currentState.pure
        case (currentState, TextFieldEvent.SelectAll) if currentState.isFocused(path)  =>
          currentState.selectAll().pure
        case (currentState, TextFieldEvent.GainedFocus(pathToFocusOn)) =>
          currentState.focus(pathToFocusOn).pure
        case (currentState, _) =>
          currentState.pure
      }
    },
    body = body
  )
end textField

def basicTextFieldBody[
  Widget,
  PlacedText
](
   textPlacer : (state :TextFieldState, body : PlacedText => Widget) => Widget,
   systemEventCatcher : PlacedText => Widget => Widget,
   text : PlacedText => Widget,
 )(
   state : TextFieldState
 ) : Widget =
  textPlacer(
    state,
    placedText =>
      systemEventCatcher(placedText)(
        text(placedText),
      )
  )
end basicTextFieldBody
