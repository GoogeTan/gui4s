package gui4s.core.widget.library

import catnip.BiMonad
import cats.*
import cats.data.*
import cats.syntax.all.*
import gui4s.core.widget.Path

import scala.math.min

final case class TextFieldState(
                                 lines: List[String] = Nil,
                                 anchor: TextPosition = TextPosition.Zero,
                                 cursor: TextPosition = TextPosition.Zero,
                                 focusedTextField: Option[Path] = None
                               ):
  def this(lines : String, anchor : TextPosition, cursor : TextPosition, focusedTextField : Option[Path]) =
    this(lines.split("\n").toList, anchor, cursor)
  end this  
  
  def this(lines : String, anchor : TextPosition, cursor : TextPosition) = 
    this(lines, anchor, cursor, None)
  end this  
  
  def this(lines : String, cursor : TextPosition) = 
    this(lines, cursor, cursor)
  end this
  
  def this(lines : String) = 
    this(lines, TextPosition.Zero)
  end this  
  
  def cursorStartIndex : Int =
    if cursor.line != 0 then
      lines.take(cursor.line).mkString("", "\n", "\n").length + cursor.column
    else
      cursor.column
    end if
  end cursorStartIndex

  private def normalize: (TextPosition, TextPosition) =
    if anchor < cursor then
      (anchor, cursor)
    else
      (cursor, anchor)
  end normalize

  def insert(text: String): TextFieldState =
    val (start, end) = normalize
    val deleted = deleteRange(start, end)
    deleted.insertAtCursor(text)
  end insert

  def selectAll(): TextFieldState =
    copy(anchor = TextPosition.Zero, cursor = TextPosition(lines.size - 1, lines.last.length))
  end selectAll

  private def insertAtCursor(text: String): TextFieldState =
    val before = lines(cursor.line).take(cursor.column)
    val after = lines(cursor.line).drop(cursor.column)
    val parts = text.split("\n", -1).toSeq
    val newSegment: Seq[String] = if (parts.isEmpty)
      Seq(before + after)
    else if (parts.size == 1)
      Seq(before + parts.head + after)
    else
      (before + parts.head) +: parts.slice(1, parts.size - 1) :+ (parts.last + after)
    val newLines = lines.patch(cursor.line, newSegment, 1)
    val newLine = cursor.line + (parts.size - 1)
    val newCol =
      if parts.size == 1 then
        cursor.column + parts.head.length
      else if parts.nonEmpty then
        parts.last.length
      else
        0
    val newCursor = TextPosition(newLine, newCol)
    copy(lines = newLines, anchor = newCursor, cursor = newCursor)
  end insertAtCursor

  def backspace(): TextFieldState =
    if (anchor == cursor)
      if cursor.column > 0 then
        val column = min(cursor.column, lines(cursor.line).length)
        val newLine = lines(cursor.line).patch(column - 1, "", 1)
        copy(
          lines = lines.updated(cursor.line, newLine),
          anchor = TextPosition(cursor.line, column - 1),
          cursor = TextPosition(cursor.line, column - 1)
        )
      else if cursor.line > 0 then
        val prevLine = lines(cursor.line - 1)
        val newPrev = prevLine + lines(cursor.line)
        val newLines = lines.patch(cursor.line - 1, Seq(newPrev), 2)
        val newPos = TextPosition(cursor.line - 1, prevLine.length)
        copy(lines = newLines, anchor = newPos, cursor = newPos)
      else this
    else
      val (start, end) = normalize
      deleteRange(start, end)
  end backspace

  def delete(): TextFieldState =
    if (anchor == cursor)
      if (cursor.column < lines(cursor.line).length)
        val newLine = lines(cursor.line).patch(cursor.column, "", 1)
        copy(lines = lines.updated(cursor.line, newLine))
      else if (cursor.line < lines.size - 1)
        val nextLine = lines(cursor.line + 1)
        val newCurrent = lines(cursor.line) + nextLine
        val newLines = lines.patch(cursor.line, Seq(newCurrent), 2)
        copy(lines = newLines)
      else this
    else
      val (start, end) = normalize
      deleteRange(start, end)
    end if
  end delete

  def deleteRange(start: TextPosition, end: TextPosition): TextFieldState =
    if (start.line == end.line)
      val newLine = lines(start.line).patch(start.column, "", end.column - start.column)
      copy(lines = lines.updated(start.line, newLine), anchor = start, cursor = start)
    else
      val firstPart = lines(start.line).take(start.column)
      val lastPart = lines(end.line).drop(end.column)
      val newLine = firstPart + lastPart
      val newLines = lines.patch(start.line, Seq(newLine), end.line - start.line + 1)
      new TextFieldState(newLines, TextPosition(start.line, start.column))
  end deleteRange

  def moveCursorLeft(shift: Boolean): TextFieldState =
    val newCursor = if (cursor.column > 0)
      TextPosition(cursor.line, cursor.column - 1)
    else if (cursor.line > 0)
      TextPosition(cursor.line - 1, lines(cursor.line - 1).length)
    else cursor
    updatePositions(shift, newCursor)
  end moveCursorLeft

  def moveCursorRight(shift: Boolean): TextFieldState =
    val newCursor = if (cursor.column < lines(cursor.line).length)
      TextPosition(cursor.line, cursor.column + 1)
    else if (cursor.line < lines.size - 1)
      TextPosition(cursor.line + 1, 0)
    else cursor
    updatePositions(shift, newCursor)
  end moveCursorRight

  def moveCursorUp(shift: Boolean): TextFieldState =
    if cursor.line == 0 then
      this
    else
      val newCol = min(cursor.column, lines(cursor.line - 1).length)
      val newCursor = TextPosition(cursor.line - 1, newCol)
      updatePositions(shift, newCursor)
    end if
  end moveCursorUp

  def moveCursorDown(shift: Boolean): TextFieldState =
    if cursor.line == lines.size - 1 then
      this
    else
      val newCol = min(cursor.column, lines(cursor.line + 1).length)
      val newCursor = TextPosition(cursor.line + 1, newCol)
      updatePositions(shift, newCursor)
    end if
  end moveCursorDown

  private def updatePositions(shift: Boolean, newCursor: TextPosition): TextFieldState =
    if (shift)
      if (anchor == cursor)
        copy(cursor = newCursor, anchor = cursor)
      else
        copy(cursor = newCursor)
      end if
    else
      copy(anchor = newCursor, cursor = newCursor)
    end if
  end updatePositions

  def getBefore: String =
    val (start, _) = normalize
    val beforeLines = lines
      .take(start.line)
      .mkString("\n") + (if (start.line > 0) "\n" else "")
      + lines(start.line).take(start.column)
    beforeLines
  end getBefore

  def getSelected: String =
    val (start, end) = normalize
    if (start == end)
      ""
    else if (start.line == end.line)
      lines(start.line).substring(start.column, end.column)
    else
      val selected = Seq(lines(start.line).drop(start.column)) ++
        lines.slice(start.line + 1, end.line) ++
        Seq(lines(end.line).take(end.column))
      selected.mkString("\n")
    end if
  end getSelected

  def getAfter: String =
    val (_, end) = normalize
    lines(end.line).drop(end.column)
      + (if (end.line < lines.size - 1) "\n" else "")
      + lines.drop(end.line + 1).mkString("\n")
  end getAfter

  def getText : String =
    lines.mkString("\n")
  end getText

  override def equals(obj: Any): Boolean =
    obj match
      case other : TextFieldState =>
        this.lines == other.lines && this.anchor == other.anchor && this.cursor == other.cursor && this.focusedTextField == other.focusedTextField
      case _ => false
    end match
  end equals

  def putWholeCursorTo(pos : TextPosition) : TextFieldState =
    copy(cursor = pos, anchor = pos)
  end putWholeCursorTo

  def putCursorTo(pos : TextPosition) : TextFieldState =
    copy(cursor = pos)
  end putCursorTo

  def textPositionOf(position : Int) : TextPosition =
    lines.foldLeft[Either[(Int, Int), TextPosition]](Left((0, 0)))((res : Either[(Int, Int), TextPosition], line : String) =>
      res match
        case Right(res : TextPosition) => Right(res)
        case Left((lines, linesSum)) =>
          if linesSum + line.length > position then
            Right(TextPosition(lines, position - linesSum))
          else
            Left((lines + 1, linesSum + line.length))
    ).fold(
      _ => TextPosition(lines.length - 1, lines.last.length),
      identity
    )
  end textPositionOf

  def focus(pathToFocusOn : Path) : TextFieldState =
    copy(focusedTextField = Some(pathToFocusOn))
  end focus

  def isFocused(currentPath : Path) : Boolean =
    focusedTextField.contains(currentPath)
  end isFocused
end TextFieldState

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
    eventHandler = { (stateDontTouch : TextFieldState, path, events : NonEmptyList[TextFieldEvent]) =>
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
