package me.katze.gui4s.example

import impl.{ENErrors, WindowResized, containerPlacementCurried}
import update.ApplicationRequest

import cats.effect.IO
import cats.syntax.all.*
import me.katze.gui4s.example.draw.{*, given}
import me.katze.gui4s.example.draw.swing.{*, given}
import me.katze.gui4s.impure.cats.effect.IOImpure
import me.katze.gui4s.layout.{MeasurableT, given}
import me.katze.gui4s.widget.{EventResult, given}
import me.katze.gui4s.example.impl.{*, given}
import me.katze.gui4s.example.{*, given}
import impl.{*, given}

import me.katze.gui4s.example.task.{EventProducingEffectT, RunnableIO}

type Update[+Task] = [A, B] =>> EventResult[Task, A, B]
type Task[T] = RunnableIO[EventProducingEffectT[IO], T]
type Recomposition = IO[Unit]

@SuppressWarnings(Array("org.wartremover.warts.Any"))
abstract class SwingGui4sApp extends Gui4sApp[MeasurableT[Float], Update[Task[Any]], Recomposition, Task, Float, SwingDraw[IO, Float, Unit], Any](
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
  SkijaMeasurableRunPlacement(_),
  a => a,
  [T] => (update : Update[Task[Any]][T, ApplicationRequest]) => Right(update.widget).pure[IO]
)

