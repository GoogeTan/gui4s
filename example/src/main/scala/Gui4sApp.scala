package me.katze.gui4s.example

import api.impl.{HighLevelApiImpl, LayoutPlacement, LayoutPlacementMeta}
import api.{HighLevelApi, TextWidgetApi, LayoutApi}
import draw.*
import impl.{*, given}
import place.*
import update.*

import cats.*
import cats.effect.*
import cats.effect.std.{Queue, QueueSink}
import cats.syntax.all.*
import me.katze.gui4s.layout.bound.Bounds
import me.katze.gui4s.widget
import me.katze.gui4s.widget.library.{TextDraw, LayoutDraw, LiftEventReaction, TextPlacement}
import me.katze.gui4s.widget.stateful.{BiMonad, CatchEvents, Path, RaiseEvent}

trait Gui4sApp[
  Place[+_] : FlatMap,
  Update[+_, +_] : {BiMonad, CatchEvents, RaiseEvent},
  Recomposition : {Monoid},
  Task[+_],
  MeasurementUnit : Fractional,
  Draw : Monoid,
  TextStyle
](
    val drawApi: QueueSink[IO, DownEvent] => Resource[IO, (
        IO[Bounds[MeasurementUnit]],
        DrawLoop[IO, Drawable[Draw]],
        LayoutDraw[Draw, LayoutPlacementMeta[MeasurementUnit]],
        TextDraw[Draw, LayoutPlacementMeta[MeasurementUnit]],
      )
    ],
    val containerPlacement : LayoutPlacement[Update, Draw, Place, Recomposition, DownEvent, MeasurementUnit],
    val runPlacement : IO[Bounds[MeasurementUnit]] => RunPlacement[IO, Place],
    val runRecomposition : Recomposition => IO[Unit],
    val runUpdate: [A] => Update[A, ApplicationRequest] => IO[Either[ExitCode, A]]
)(
  using
  TextPlacement[Place[LayoutPlacementMeta[MeasurementUnit]], TextStyle],
  LiftEventReaction[Update, Task[Any]]
) extends IOApp:
  def rootWidget[T <: HighLevelApi & LayoutApi[MeasurementUnit] & TextWidgetApi[TextStyle]](using api: T) : api.Widget[ApplicationRequest]
  
  final override def run(args: List[String]): IO[ExitCode] =
    for
      eventBus <- Queue.unbounded[IO, DownEvent]
      code <- drawApi(eventBus).use((windowSize, drawLoop, layoutDraw, textDraw) =>
        given layoutDraw.type = layoutDraw
        given textDraw.type = textDraw
        val widgetApi = new HighLevelApiImpl(containerPlacement)
        given RunPlacement[IO, Place] = runPlacement(windowSize)
        for
          rootWidget <- rootWidget[widgetApi.type](using widgetApi).runPlacement
          widget <- Ref[IO].of(RootWidget(Path(List("ROOT")), rootWidget, runRecomposition))
          code <- applicationLoop(eventBus, widget, drawLoop, updateLoop(runUpdate)).flatMap(_.join)
        yield code
      )
    yield code
  end run
end Gui4sApp


