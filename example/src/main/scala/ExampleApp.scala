package me.katze.gui4s.example

import api.*
import draw.swing.{SwingApi, SwingDraw, SwingDrawT, runSwingDraw, swingTextPlacement, given}

import impl.LayoutPlacementMeta
import update.ApplicationRequest

import cats.Applicative
import cats.effect.IO
import me.katze.gui4s.impure.cats.effect.IOImpure
import me.katze.gui4s.widget.library.*
import me.katze.gui4s.example.impl.{RecompositionAction, WindowResized, containerPlacementCurried, layoutDrawImpl, runRecompositionActionInTaskSet, given}
import me.katze.gui4s.example.place.MainAxisStrategyErrors
import me.katze.gui4s.example.task.{EventProducingEffectT, RunnableIO, TaskSet}
import me.katze.gui4s.layout.{MeasurableT, given}
import me.katze.gui4s.widget.{EventResult, given}
import me.katze.gui4s.widget.stateful.KillTasks
import cats.syntax.all.*

given ld[Draw, MeasurementUnit : Numeric]: LayoutDraw[SwingDraw[IO, MeasurementUnit, Unit], LayoutPlacementMeta[MeasurementUnit]] = layoutDrawImpl[SwingDrawT[IO, MeasurementUnit], MeasurementUnit]

type Update[+Task] = [A, B] =>> EventResult[Task, A, B]
type Task[T] = RunnableIO[EventProducingEffectT[IO], T]
type Recomposition = List[RecompositionAction[RunnableIO[EventProducingEffectT[IO], Any]]]
given KillTasks[Recomposition] = path => List(RecompositionAction.KillTasksFor(path))

def runRecompositionInTaskSet[F[_] : Applicative, Task](taskSet: TaskSet[F, Task], recomposition: List[RecompositionAction[Task]]) : F[Unit] =
  recomposition.traverse_(runRecompositionActionInTaskSet(taskSet, _))
end runRecompositionInTaskSet

val ENErrors = MainAxisStrategyErrors(
  "Tried to place elements in layout with Center mode. It requires container to be finite but infinite container found. You have tried to place something in the middle of infinity xD",
  "Tried to place elements in layout with End mode. It requires container to be finite but infinite container found. You have tried to place something in the end of infinity xD",
  "Tried to place elements in layout with SpaceAround mode. It requires container to be finite but infinite container found. You have tried to place elements with infinite space around them xD",
  "Tried to place elements in layout with SpaceBetween mode. It requires container to be finite but infinite container found. You have tried to place elements with infinite space between them xD",
)

@SuppressWarnings(Array("org.wartremover.warts.Any"))
object ExampleApp extends Gui4sApp[MeasurableT[Float], Update[Task[Any]], Recomposition, Task, Float, SwingDraw[IO, Float, Unit], Any](
  queue => SwingApi[IO, Float, SwingDrawT[IO, Float]](IOImpure, queue.offer(WindowResized)),
  runSwingDraw,
  containerPlacementCurried(ENErrors),
  MeasurableRunPlacement(_),
  runRecompositionInTaskSet
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

