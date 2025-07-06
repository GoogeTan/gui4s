package me.katze.gui4s.example

import api.exported.{*, given}
import api.{TextWidget, makeSkijaStatefulWidget, makeSkijaTextWidget}
import draw.skija.SkijaBackend
import impl.ENErrors
import place.MainAxisStrategyErrors
import update.ApplicationRequest

import catnip.FFI
import catnip.cats.effect.SyncFFI
import catnip.syntax.all.given
import cats.data.*
import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all.*
import io.github.humbleui.skija.{Font, Paint, Typeface}
import me.katze.gui4s
import me.katze.gui4s.example
import me.katze.gui4s.glfw.OglWindow
import me.katze.gui4s.layout.{Point2d, Sized, given}
import me.katze.gui4s.skija.{SkijaDraw, SkijaDrawState, SkijaTextStyle}
import me.katze.gui4s.widget.library.{*, given}
import me.katze.gui4s.widget.{EventReaction, Path}

import scala.annotation.experimental
import scala.language.experimental.namedTypeArguments

@experimental
object SkijaAppExample extends IOApp:
  given MainAxisStrategyErrors = ENErrors
  given ffi : FFI[IO] = SyncFFI[IO]

  private type Widget[Event] =  SkijaWidget[IO, Float, String, Event, SkijaDownEvent]

  override def run(args: List[String]): IO[ExitCode] =
    skijaApp[IO, String](
      widget = main, 
      updateLoopExecutionContext = this.runtime.compute,
      drawLoopExecutionContext = MainThread,
      runEitherTError = [V] => (value : EitherT[IO, String, V]) =>
        value.value.flatMap:
          case Left(error) =>
            IO.raiseError(new Exception(error))
          case Right(value) =>
            IO.pure(value)
    )
  end run

  def eventCatcher[Event]: EventCatcherWithRect[Widget[Event], SkijaUpdate[Float, Event, Boolean], Float, SkijaDownEvent] = eventCatcherWithWidgetsRect(
    markEventHandled,
    getCoordinates,
  )

  def mouseTracker[Event](name : String) : WithContext[Widget[Event], Option[Point2d[Float]]] =
    rememberLastEventOfTheType[Widget = Widget, Event = Event, MemorableEvent = Point2d[Float], Update = SkijaUpdate[Float, *, Boolean]](
      eventCatcherWithRect = eventCatcher,
      statefulWidget = transitiveStatefulWidget,
      mapUpdate = [A, B] => f => mapEvents(f),
      mapEvent = mapEvent[Update = SkijaUpdate[Float, *, *]]([T, A, B] => f => mapEvents(f)),
      name = name,
      catchEvent =
        (path, rect, event) =>
          event match
            case SkijaDownEvent.MouseMove(x, y) =>
              raiseEvents[Float, Point2d[Float]](List(Point2d(x.toFloat, y.toFloat))).as(false)
            case _ => false.pure
    )
  end mouseTracker

  def clickHandler[Event](name : String): ClickHandler[Widget[Event], SkijaUpdate[Float, Event, Boolean], Unit] =
    makeClickHandler(
      eventCatcherWithRect = eventCatcher,
      mouseTracker = mouseTracker(name).map(_.getOrElse(Point2d(0, 0)))
    )(
      extractClickHandlerEvent
    )

  extension[Event](widget : Widget[Event])
    def onClick(name : String)(event : Event) : Widget[Event] =
      clickHandler(name)(widget)(
        (_, _) =>
          raiseEvents[Float, Event](List(event)).as(true)
      )
    end onClick
  end extension
  
  
  def statefulWidget: StatefulWidget[Widget, Nothing] = makeSkijaStatefulWidget(
    (value: Any, path: Path) => "Error in stateful typechecking at " + path.toString + " with value [" + value.toString + "]"
  )

  def transitiveStatefulWidget: TransitiveStatefulWidget[Widget, Nothing] = TransitiveStatefulWidgetFromStatefulWidget(statefulWidget)
  
  def text(using backend : SkijaBackend[IO, OglWindow]) : TextWidget[Widget] =
    makeSkijaTextWidget(backend.globalShaper, ffi, backend.globalTextCache)
  end text

  def leaf[Marker, Event](marker : Marker) : Widget[Event] =
    leafWidget[
      Update = SkijaUpdateT[Float, Event],
      Place = SkijaPlaceT[IO, Float, String]
    ](
      new Sized(
        marker,
        0f,
        0f
      ).pure[SkijaPlaceInnerT[IO, Float, String]],
      ReaderT.pure[IO, SkijaDrawState[IO, OglWindow], Unit](()),
      ().pure
    )
  end leaf

  def main(using SkijaBackend[IO, OglWindow]) : Widget[ApplicationRequest] =
    app((0 until 6).toList)
  end main

  def app(numbers : List[Int])(using SkijaBackend[IO, OglWindow]) : Widget[ApplicationRequest] =
    skijaColumn(
      verticalStrategy = MainAxisPlacementStrategy.Begin(0),
      horizontalStrategy = AdditionalAxisPlacementStrategy.Center,
      children = List(
        mouseTracker("mouseTracker")(
          maybeMousePoint =>
            text(
              maybeMousePoint.map(point => "x: " + point.x.toString + " y: " + point.y.toString).getOrElse("No movement"),
              SkijaTextStyle(new Font(Typeface.makeDefault(), 26), new Paint().setColor(0xFF8484A4))
            )
        )
      ) ++ numbers.map:
        lineNumber =>
          statefulWidget[Int, ApplicationRequest, Unit](
            name = "line-" + lineNumber.toString,
            initialState = 0,
            eventHandler = (state, _) =>
              EventReaction(state + 1, Nil, Nil),
            body = state =>
              text(
                "# " + lineNumber.toString + " : " + state.toString,
                SkijaTextStyle(new Font(Typeface.makeDefault(), 26), new Paint().setColor(0xFF8484A4))
              ).onClick("click_handler_" + lineNumber.toString)(())
          ),
    )
  end app
end SkijaAppExample

@experimental
def extractClickHandlerEvent(downEvent : SkijaDownEvent) : Option[Unit] =
  downEvent match
    case SkijaDownEvent.MouseClick(button, action, mods) =>
      Some(()) // TODO ClickHandlerDownEvent(button, action, mods))
    case _ => None
end extractClickHandlerEvent