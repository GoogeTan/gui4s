package gui4s.core.widget.library.textfield

import gui4s.core.widget.Path

enum TextFieldEvent:
  case CharInput(char : Char)
  case Backspace
  case GoLeft(shift : Boolean = false)
  case GoRight(shift : Boolean = false)
  case GoUp(shift : Boolean = false)
  case GoDown(shift : Boolean = false)
  case MoveWholeCursorTo(pos : Int)
  case MoveCursorTo(pos : Int)
  case ClipboardPaste(text : String)
  case ClipboardCopy
  case ClipboardCut
  case SelectAll
  case Undo
  case Redo
  case GainedFocus(pathToFocusOn : Path)
end TextFieldEvent
