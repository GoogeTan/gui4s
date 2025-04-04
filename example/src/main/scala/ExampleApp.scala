package me.katze.gui4s.example

import api.*
import api.impl.LayoutPlacementMeta
import draw.swing.{*, given}
import draw.{Drawable, SimpleDrawApi, TextStyle}
import place.MainAxisStrategyErrors
import task.{EventProducingEffectT, RunnableIO, TaskSet}
import update.ApplicationRequest

import cats.effect.std.Console
import cats.effect.{Async, ExitCode, IO}
import cats.kernel.Semigroup
import cats.syntax.all.*
import cats.{Applicative, Functor, data}
import me.katze.gui4s.example.impl.{*, given}
import me.katze.gui4s.example.{*, given}
import me.katze.gui4s.impure.cats.effect.IOImpure
import me.katze.gui4s.layout.{MeasurableT, given}
import me.katze.gui4s.widget.library.*
import me.katze.gui4s.widget.{EventResult, given}

given ld[Draw, MeasurementUnit : Numeric]: LayoutDraw[SwingDraw[IO, MeasurementUnit, Unit], LayoutPlacementMeta[MeasurementUnit]] = layoutDrawImpl[SwingDrawT[IO, MeasurementUnit], MeasurementUnit]
given textDraw[MeasurementUnit](using api : SimpleDrawApi[MeasurementUnit, SwingDraw[IO, MeasurementUnit, Unit]]): LabelDraw[SwingDraw[IO, MeasurementUnit, Unit], LayoutPlacementMeta[MeasurementUnit]] =
  (text, meta) =>
    api.text(meta.x, meta.y, text, TextStyle(18, 0, 400))
end textDraw

type Update[+Task] = [A, B] =>> EventResult[Task, A, B]
type Task[T] = RunnableIO[EventProducingEffectT[IO], T]
type Recomposition = IO[Unit]

val ENErrors = MainAxisStrategyErrors(
  "Tried to place elements in layout with Center mode. It requires container to be finite but infinite container found. You have tried to place something in the middle of infinity xD",
  "Tried to place elements in layout with End mode. It requires container to be finite but infinite container found. You have tried to place something in the end of infinity xD",
  "Tried to place elements in layout with SpaceAround mode. It requires container to be finite but infinite container found. You have tried to place elements with infinite space around them xD",
  "Tried to place elements in layout with SpaceBetween mode. It requires container to be finite but infinite container found. You have tried to place elements with infinite space between them xD",
)

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

@SuppressWarnings(Array("org.wartremover.warts.Any"))
object ExampleApp extends Gui4sApp[MeasurableT[Float], Update[Task[Any]], Recomposition, Task, Float, SwingDraw[IO, Float, Unit], Any](
  queue =>
    SwingApi[IO, Float, SwingDrawT[IO, Float]](IOImpure, queue.offer(WindowResized)).map(api =>
      (api.windowBounds, simpleGraphicsDrawLoop[IO, Float, SwingDraw[IO, Float, Unit]](api.graphics, runSwingDraw), ld, textDraw(using api.graphics))
    ),
  containerPlacementCurried(ENErrors),
  MeasurableRunPlacement(_),
  a => a,
  [T] => (update : Update[Task[Any]][T, ApplicationRequest]) => Right(update.widget).pure[IO]
):
  override def rootWidget[T <: HighLevelApi & LabelApi[Unit] & LayoutApi[Float]](using api : T): api.Widget[ApplicationRequest] =
    api.column(
      List(
        api.label("12345", ()),
        api.label("12345", ()),
        api.label("12345", ()),
        api.label("12345", ()),
        api.label("12345", ()),
        api.label("12345", ())
      ),
      MainAxisPlacementStrategy.SpaceBetween,
      AdditionalAxisPlacementStrategy.Begin
    )
  end rootWidget
end ExampleApp

