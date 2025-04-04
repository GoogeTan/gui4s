package me.katze.gui4s.example
package draw

import cats.Functor
import cats.effect.{Async, ExitCode}
import cats.effect.std.Console
import cats.kernel.Semigroup
import cats.syntax.all.*

trait SimpleDrawApi[MeasurementUnit, F]:
  def text(x : MeasurementUnit, y : MeasurementUnit, text : String, style: TextStyle) : F
  
  def rectangle(x : MeasurementUnit, y : MeasurementUnit, width : MeasurementUnit, height : MeasurementUnit, color : Int) : F
  
  def beginDraw : F
  def endDraw : F
  
  def drawFrame(frame : F)(using m : Semigroup[F]) : F =
    beginDraw |+| frame |+| endDraw
  end drawFrame
end SimpleDrawApi

def simpleGraphicsDrawLoop[F[+_] : {Async, Console}, MeasurementUnit, Draw : Semigroup](graphics: SimpleDrawApi[MeasurementUnit, Draw], runDraw: Draw => F[Unit]) : DrawLoop[F, Drawable[Draw]] =
  currentWidget =>
    drawLoop(drawLoopExceptionHandler)(
      currentWidget.map(widget => graphics.drawFrame(widget.draw)).flatMap(runDraw)
    )
end simpleGraphicsDrawLoop

// TODO Почему-то ругается на эни в интерполяции строки...
@SuppressWarnings(Array("org.wartremover.warts.Any"))
def drawLoopExceptionHandler[F[_] : Functor](exception: Throwable)(using c : Console[F]): F[Option[ExitCode]] =
  c.println(s"Error in draw loop: $exception").map(_ => Some(ExitCode.Error))
end drawLoopExceptionHandler
