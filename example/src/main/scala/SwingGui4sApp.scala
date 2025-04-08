package me.katze.gui4s.example

import draw.swing.{*, given}
import draw.{*, given}
import update.ApplicationRequest

import cats.Id
import cats.effect.IO
import cats.syntax.all.*
import impl.{ENErrors, WindowResized, *}

import me.katze.gui4s.example.{*, given}
import impl.{*, given}
import task.{EventProducingEffectT, RunnableIO}

import me.katze.gui4s.impure.cats.effect.IOImpure
import me.katze.gui4s.layout.{MeasurableT, given}
import me.katze.gui4s.widget.{EventResult, given}

type Update[+Task] = [A, B] =>> EventResult[Task, A, B]
type Task[T] = RunnableIO[EventProducingEffectT[IO], T]
type Recomposition = IO[Unit]

@SuppressWarnings(Array("org.wartremover.warts.Any"))
abstract class SwingGui4sApp extends Gui4sApp[MeasurableT[Id, Float], Update[Task[Any]], Recomposition, Task, Float, SwingDraw[IO, Float, Unit], Any](
  queue =>
    SwingApi[IO, Float, SwingDraw[IO, Float, Unit]](IOImpure, queue.offer(WindowResized)).map(api =>
      (
        api.windowBounds,
        simpleGraphicsDrawLoop[IO, Float, SwingDraw[IO, Float, Unit]](api.graphics, runSwingDraw),
        swingLayoutDraw[SwingDraw[IO, Float, Unit], Float],
        swingTextDraw(using api.graphics)
      )
    ),
  containerPlacementCurried(ENErrors),
  MeasurableRunPlacement(_),
  a => a,
  [T] => (update : Update[Task[Any]][T, ApplicationRequest]) => Right(update.widget).pure[IO]
)

