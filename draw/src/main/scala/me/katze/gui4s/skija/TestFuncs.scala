package me.katze.gui4s.skija

import cats.Monad
import cats.syntax.all.*
import io.github.humbleui.skija.{Canvas, Paint, TextBlob}
import io.github.humbleui.types.Rect
import me.katze.gui4s.impure.Impure

final class TestFuncs[F[_] : {Impure as I, Monad}]:
  def drawString(canvas: Canvas, text : TextBlob, paint: Paint) : F[Unit] =
    I:
      canvas.drawTextBlob(text, 0, 0, paint)
  end drawString

  def saveState(canvas: Canvas) : F[Int] =
    I(canvas.save())
  end saveState
  
  def loadState(canvas: Canvas, state : Int) : F[Unit] =
    I(canvas.restoreToCount(state))
  end loadState
  
  def transact[T](canvas: Canvas, value : F[T]) : F[T] =
    for
      state <- saveState(canvas)
      res <- value 
      _ <- loadState(canvas, state)
    yield res
  end transact
  
  def transition(canvas : Canvas, x : Float, y : Float) : F[Unit] =
    I:
      canvas.translate(x, y)
  end transition

  def drawRect(canvas: Canvas, width : Float, height : Float, paint : Paint) : F[Unit] =
    I:
      canvas.drawRect(Rect.makeWH(width, height), paint)
  end drawRect
  
  def moveAndBack[T](canvas: Canvas, x : Float, y : Float, value : F[T]) : F[T] =
    transition(canvas, x, y) *> value <* transition(canvas, -x, -y)
  end moveAndBack
end TestFuncs

