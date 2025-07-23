package me.katze.gui4s.example

import api.exported.{*, given}
import api.{TextWidget, makeSkijaStatefulWidget, makeSkijaTextWidget}
import draw.skija.SkijaBackend
import impl.ENErrors
import place.ElementPlacementInInfiniteContainerAttemptError
import update.ApplicationRequest

import catnip.ForeighFunctionInterface
import catnip.cats.effect.SyncForeighFunctionInterface
import catnip.syntax.all.given
import cats.data.*
import cats.effect.std.Supervisor
import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all.*
import io.github.humbleui.skija.{Font, Paint, Typeface}
import me.katze.gui4s
import me.katze.gui4s.example
import me.katze.gui4s.glfw.KeyAction.Press
import me.katze.gui4s.glfw.{GlfwWindow, OglGlfwWindow}
import me.katze.gui4s.layout.{Point2d, Sized, given}
import me.katze.gui4s.skija.{Pixel, SkijaDraw, SkijaDrawState, SkijaTextStyle}
import me.katze.gui4s.widget.library.{*, given}
import me.katze.gui4s.widget.{EventReaction, Path, library}

import scala.annotation.experimental
import scala.reflect.Typeable

@experimental
object SkijaAppExample extends IOApp:
  given ElementPlacementInInfiniteContainerAttemptError[String] = ENErrors
  given ffi : ForeighFunctionInterface[IO] = SyncForeighFunctionInterface[IO]

  private type Widget[Event] = SkijaWidget[IO, Pixel, String, String, Event, SkijaDownEvent]

  override def run(args: List[String]): IO[ExitCode] =
    skijaApp[IO, String, String](
      widget = main, 
      updateLoopExecutionContext = this.runtime.compute,
      drawLoopExecutionContext = MainThread,
      updateErrorAsExitCode = error => IO.println(error).as(ExitCode.Error),
      runEitherTError = [V] => (value : EitherT[IO, String, V]) =>
        value.value.flatMap:
          case Left(error) =>
            IO.raiseError(new Exception(error))
          case Right(value) =>
            IO.pure(value)
    )

  def eventCatcher[Event]: EventCatcherWithRect[Widget[Event], SkijaUpdate[IO, String, Pixel, Event, Boolean], Pixel, SkijaDownEvent] = eventCatcherWithWidgetsRect(
    markEventHandled,
    getCoordinates,
  )

  def mouseTracker[Event](name : String) : WithContext[Widget[Event], Option[Point2d[Pixel]]] =
    rememberLastEventOfTheType[Widget, SkijaUpdate[IO, String, Pixel, *, Boolean], Path => IO[Unit], Pixel, Event, SkijaDownEvent, Point2d[Pixel]](
      eventCatcherWithRect = eventCatcher,
      statefulWidget = transitiveStatefulWidget,
      mapUpdate = [A, B] => f => mapEvents(f),
      mapEvent = library.mapEvent2([T, A, B] => f => mapEvents(f)),
      name = name,
      catchEvent =
        (path, rect, event) =>
          event match
            case SkijaDownEvent.MouseMove(x, y) =>
              raiseEvents[IO, String, Pixel, Point2d[Pixel]](List(Point2d(x, y))).as(false)
            case _ => false.pure[SkijaUpdateT[IO, String, Pixel, Point2d[Pixel]]]
    )
  end mouseTracker

  def mmapEvent : MapEvent[Widget] =
    library.mapEvent2([T, A, B] => (f : A => B) => mapEvents(f))

  extension[Event](value : Widget[Event])
    def mapEvent[NewEvent](f : Event => NewEvent) : Widget[NewEvent] =
      mmapEvent.mapEvent(value)(f)
    end mapEvent
  end extension

  def clickHandler[Window <: GlfwWindow[IO, Monitor], Event, Monitor](window : Window): ClickHandler[Widget[Event], SkijaUpdate[IO, String, Pixel, Event, Boolean], Unit] =
    makeClickHandler(
      eventCatcherWithRect = eventCatcher,
      currentMousePosition = liftIOToSkijaUpdate(window.currentMousePosition.map((x, y) => Point2d(Pixel(x.toFloat), Pixel(y.toFloat)))),
    )(
      extractClickHandlerEvent
    )

  extension[Event](widget : Widget[Event])
    def onClick[Window <: GlfwWindow[IO, Monitor], Monitor](window : Window)(event : Event) : Widget[Event] =
      clickHandler(window)(widget)(
        (_, _) =>
          raiseEvents[IO, String, Pixel, Event](List(event)).as(true)
      )
    end onClick
  end extension
  
  def statefulWidget: StatefulWidget[Widget, Path => IO[Unit]] = makeSkijaStatefulWidget(
    (value: Any, path: Path) => "Error in stateful typechecking at " + path.toString + " with value [" + value.toString + "]",
  )

  def transitiveStatefulWidget: TransitiveStatefulWidget[Widget, Path => IO[Unit]] = TransitiveStatefulWidgetFromStatefulWidget(statefulWidget)
  
  def text[Window <: GlfwWindow[IO, Monitor], Monitor](using backend : SkijaBackend[IO, Window, Monitor]) : TextWidget[Widget] =
    makeSkijaTextWidget(backend.globalShaper, ffi, backend.globalTextCache)
  end text

  def launchedEffect[Event, Key : Typeable](supervisor : Supervisor[IO]) : LaunchedEffectWidget[Widget[Event], Key, Path => IO[Unit]] =
    val lew : LaunchedEffectWidget[Widget[Event], Key, Path => SkijaRecomposition[IO]] = library.launchedEffect(
      [T] => (path : Path) => raiseError("Key has changed type at " + path.toString),
      IO.raiseError(Exception("Key changed the type"))
    )
    (name, child, key, task) =>
      lew(name, child, key, path => supervisor.supervise(task(path)).as(()))

  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf", "org.wartremover.warts.Any"))
  given destructableIsTypeable[T : Typeable] : Typeable[(T, IO[Unit])] = {
    case (a: T, io: IO[t]) => Some[(T, IO[Unit])]((a, io.as(()))).map(_.asInstanceOf)
    case _ => None
  }

  // TODO refactor me
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def resource[Window <: GlfwWindow[IO, Monitor], Monitor, Event](supervisor : Supervisor[IO], backend : SkijaBackend[IO, Window, Monitor]) : ResourceWidget[Widget[Event], IO] =
    [Value : Typeable] => (name : String, resource : IO[(Value, IO[Unit])]) =>
      (widget : Option[Value] => Widget[Event]) =>
        transitiveStatefulWidget[Option[(Value, IO[Unit])], Event, (Value, IO[Unit])](
          name = name,
          initialState = None,
          eventHandler = (state, events) =>
            (state, events) match
              case (None, NonEmptyList(event, Nil)) => EventReaction(Some(event), Nil, Nil)
              case _ => EventReaction(state, Nil, Nil), //TODO raiseErrorInUpdate[IO, String, Pixel, Event, ]("Resource was allocated twice"),
          body = state =>
            launchedEffect[Either[(Value, IO[Unit]), Event], Unit](supervisor)(
              name + "_effect_launcher",
              eventCatcher(widget(state.map(_._1)).mapEvent(Right(_)))((path, _, event) =>
                event match
                  case SkijaDownEvent.TaskRaisedEvent(taskPath, value : Any) if path == taskPath =>
                    destructableIsTypeable.unapply(value) match
                      case Some(event) => raiseEvents[IO, String, Pixel, Either[(Value, IO[Unit]), Event]](List(Left(event))).as(true)
                      case _ => false.pure[SkijaUpdateT[IO, String, Pixel, Either[(Value, IO[Unit]), Event]]]
                  case _ => false.pure[SkijaUpdateT[IO, String, Pixel, Either[(Value, IO[Unit]), Event]]]
              ),
              (),
              path => resource.flatMap(value => backend.raiseEvent(SkijaDownEvent.TaskRaisedEvent(path, value)))
            )
        )
  end resource

  def leaf[Marker, Event](marker : Marker) : Widget[Event] =
    leafWidget[
      Marker,
      SkijaUpdateT[IO, String, Pixel, Event],
      SkijaPlaceT[IO, Pixel, String],
      SkijaDraw[IO, OglGlfwWindow[IO]],
      SkijaRecomposition[IO],
      SkijaDownEvent
    ](
      new Sized(
        marker,
        Pixel(0f),
        Pixel(0f)
      ).pure[SkijaPlaceInnerT[IO, Pixel, String]],
      ReaderT.pure[IO, SkijaDrawState[IO, OglGlfwWindow[IO]], Unit](()),
      ().pure[IO]
    ).map(a => a)
  end leaf

  def main(using SkijaBackend[IO, OglGlfwWindow[IO], Long]) : Widget[ApplicationRequest] =
    app((0 until 20).toList)
  end main

  def app(numbers : List[Int])(using backend : SkijaBackend[IO, OglGlfwWindow[IO], Long]) : Widget[ApplicationRequest] =
    skijaColumn(
      verticalStrategy = MainAxisPlacementStrategy.Begin(Pixel(0f)),
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
                SkijaTextStyle(new Font(Typeface.makeDefault(), 24), new Paint().setColor(0xFF8484A4))
              ).onClick(backend.window)(())
          ),
    )
  end app

  def grid[Event](numbers : List[Int])(using SkijaBackend[IO, OglGlfwWindow[IO], Long]) : Widget[Event] =
    skijaColumn(
      verticalStrategy = MainAxisPlacementStrategy.Begin(Pixel(0f)),
      horizontalStrategy = AdditionalAxisPlacementStrategy.Begin,
      children =
        numbers.map:
          lineIndex =>
            val lineJindex = 0
            text(
              lineIndex.toString + ":" + lineJindex.toString,
              SkijaTextStyle(new Font(Typeface.makeDefault(), 28), new Paint().setColor(0xFF8484A4))
            )
    )
end SkijaAppExample

@experimental
def extractClickHandlerEvent(downEvent : SkijaDownEvent) : Option[Unit] =
  downEvent match
    case SkijaDownEvent.MouseClick(button, action, mods) if action == Press =>
      Some(()) // TODO ClickHandlerDownEvent(button, action, mods))
    case _ => None
end extractClickHandlerEvent
