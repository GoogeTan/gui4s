package me.katze.gui4s.example

import api.exported.{*, given}
import api.{TextWidget, makeSkijaStatefulWidget, makeSkijaTextWidget}
import draw.skija.SkijaBackend
import impl.ENErrors
import place.MainAxisStrategyErrors
import update.ApplicationRequest

import catnip.ForeighFunctionInterface
import catnip.cats.effect.SyncForeighFunctionInterface
import catnip.syntax.all.given
import cats.data.*
import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all.*
import io.github.humbleui.skija.{Font, Paint, Typeface}
import me.katze.gui4s
import me.katze.gui4s.example
import me.katze.gui4s.glfw.KeyAction.Press
import me.katze.gui4s.glfw.{Glfw, OglWindow}
import me.katze.gui4s.layout.{Point2d, Sized, given}
import me.katze.gui4s.skija.{SkijaDrawState, SkijaTextStyle}
import me.katze.gui4s.widget.library.{*, given}
import me.katze.gui4s.widget.{EventReaction, Path}

import scala.annotation.experimental
import scala.language.experimental.namedTypeArguments

@experimental
object SkijaAppExample extends IOApp:
  given MainAxisStrategyErrors[String] = ENErrors
  given ffi : ForeighFunctionInterface[IO] = SyncForeighFunctionInterface[IO]

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

  def eventCatcher[Event]: EventCatcherWithRect[Widget[Event], SkijaUpdate[IO, Float, Event, Boolean], Float, SkijaDownEvent] = eventCatcherWithWidgetsRect(
    markEventHandled,
    getCoordinates,
  )

  def mouseTracker[Event](name : String) : WithContext[Widget[Event], Option[Point2d[Float]]] =
    rememberLastEventOfTheType[Widget = Widget, Event = Event, MemorableEvent = Point2d[Float], Update = SkijaUpdate[IO, Float, *, Boolean]](
      eventCatcherWithRect = eventCatcher,
      statefulWidget = transitiveStatefulWidget,
      mapUpdate = [A, B] => f => mapEvents(f),
      mapEvent = me.katze.gui4s.widget.library.mapEvent2([T, A, B] => f => mapEvents(f)),
      name = name,
      catchEvent =
        (path, rect, event) =>
          event match
            case SkijaDownEvent.MouseMove(x, y) =>
              raiseEvents[IO, Float, Point2d[Float]](List(Point2d(x.toFloat, y.toFloat))).as(false)
            case _ => false.pure[SkijaUpdateT[IO, Float, Point2d[Float]]]
    )
  end mouseTracker

  def clickHandler[Window <: me.katze.gui4s.glfw.Window[IO, Monitor], Event, Monitor](window : Window): ClickHandler[Widget[Event], SkijaUpdate[IO, Float, Event, Boolean], Unit] =
    makeClickHandler(
      eventCatcherWithRect = eventCatcher,
      currentMousePosition = liftIOToSkijaUpdate(window.currentMousePosition.map((x, y) => Point2d(x.toFloat, y.toFloat))),
    )(
      extractClickHandlerEvent
    )

  extension[Event](widget : Widget[Event])
    def onClick[Window <: me.katze.gui4s.glfw.Window[IO, Monitor], Monitor](window : Window)(event : Event) : Widget[Event] =
      clickHandler(window)(widget)(
        (_, _) =>
          raiseEvents[IO, Float, Event](List(event)).as(true)
      )
    end onClick
  end extension
  
  def statefulWidget: StatefulWidget[Widget, Nothing] = makeSkijaStatefulWidget(
    (value: Any, path: Path) => "Error in stateful typechecking at " + path.toString + " with value [" + value.toString + "]"
  )

  def transitiveStatefulWidget: TransitiveStatefulWidget[Widget, Nothing] = TransitiveStatefulWidgetFromStatefulWidget(statefulWidget)
  
  def text(using backend : SkijaBackend[IO, OglWindow[IO], Long]) : TextWidget[Widget] =
    makeSkijaTextWidget(backend.globalShaper, ffi, backend.globalTextCache)
  end text

  def leaf[Marker, Event](marker : Marker) : Widget[Event] =
    leafWidget[
      Update = SkijaUpdateT[IO, Float, Event],
      Place = SkijaPlaceT[IO, Float, String],
      HandleableEvent = SkijaDownEvent
    ](
      new Sized(
        marker,
        0f,
        0f
      ).pure[SkijaPlaceInnerT[IO, Float, String]],
      ReaderT.pure[IO, SkijaDrawState[IO, OglWindow[IO]], Unit](()),
      ().pure[IO]
    ).map(a => a)
  end leaf

  def main(using SkijaBackend[IO, OglWindow[IO], Long]) : Widget[ApplicationRequest] =
    app((0 until 20).toList)
  end main

  def app(numbers : List[Int])(using backend : SkijaBackend[IO, OglWindow[IO], Long]) : Widget[ApplicationRequest] =
    skijaColumn(
      verticalStrategy = MainAxisPlacementStrategy.Begin(0),
      horizontalStrategy = AdditionalAxisPlacementStrategy.Center,
      children = numbers.map:
        lineNumber =>
          statefulWidget[Int, ApplicationRequest, Unit](
            name = "line-" + lineNumber.toString,
            initialState = 0,
            eventHandler = (state, _) => EventReaction(state + 1, Nil, Nil),
            body = state =>
              text(
                "# " + lineNumber.toString + " : " + state.toString,
                SkijaTextStyle(new Font(Typeface.makeDefault(), 48), new Paint().setColor(0xFF8484A4))
              ).onClick(backend.window)(())
          ),
    )
  end app
end SkijaAppExample

@experimental
def extractClickHandlerEvent(downEvent : SkijaDownEvent) : Option[Unit] =
  downEvent match
    case SkijaDownEvent.MouseClick(button, action, mods) if action == Press =>
      Some(()) // TODO ClickHandlerDownEvent(button, action, mods))
    case _ => None
end extractClickHandlerEvent
