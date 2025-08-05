package me.katze.gui4s.example

import api.exported.{*, given}
import place.*
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
import me.katze.gui4s.example.app.skijaGlfwApp
import me.katze.gui4s.example.skija.SkijaBackend
import me.katze.gui4s.geometry.{Axis, Point2d, Rect}
import me.katze.gui4s.glfw.KeyAction.Press
import me.katze.gui4s.glfw.{GlfwWindow, KeyAction, KeyModes, OglGlfwWindow, WindowCreationSettings}
import me.katze.gui4s.glfw.GlfwWindow.*
import me.katze.gui4s.layout.rowcolumn.{AdditionalAxisPlacement, MainAxisPlacement}
import me.katze.gui4s.layout.{Sized, given}
import me.katze.gui4s.skija.{SkijaDraw, SkijaDrawState, SkijaTextStyle, drawAt}
import me.katze.gui4s.widget.library.{*, given}
import me.katze.gui4s.widget.{EventReaction, Path, library}

import scala.annotation.experimental
import scala.reflect.Typeable

@SuppressWarnings(Array("org.wartremover.warts.Any"))
enum SkijaDownEvent[+MeasurementUnit]:
  case WindowResized extends SkijaDownEvent[Nothing]
  case MouseClick(button: Int, action: KeyAction, mods: KeyModes)
  case MouseMove(position: Point2d[MeasurementUnit])
  case KeyPress(key: Int, scancode: Int, action: KeyAction, mods: KeyModes)
  case Scrolled(xoffset: MeasurementUnit, yoffset: MeasurementUnit)
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  case TaskRaisedEvent(path: Path, event: Any)
end SkijaDownEvent

@experimental
def eventOfferingCallbacks[F, MeasurementUnit](offerEvent: SkijaDownEvent[MeasurementUnit] => F):  skija.SkijaBackend.GlfwCallbacks[F, MeasurementUnit] =
  skija.SkijaBackend.GlfwCallbacks(
    onWindowResized = _ => offerEvent(SkijaDownEvent.WindowResized),
    onMouseClick = (button, action, mods) => offerEvent(SkijaDownEvent.MouseClick(button, action, mods)),
    onMouseMove = newPosition => offerEvent(SkijaDownEvent.MouseMove(newPosition)),
    onKeyPress = (key, scancode, action, mods) => offerEvent(SkijaDownEvent.KeyPress(key, scancode, action, mods)),
    onScroll = (xoffset, yoffset) => offerEvent(SkijaDownEvent.Scrolled(xoffset, yoffset))
  )
end eventOfferingCallbacks

@experimental
object SkijaAppExample extends IOApp:
  given ElementPlacementInInfiniteContainerAttemptError[String] = ENErrors
  given ffi : ForeighFunctionInterface[IO] = SyncForeighFunctionInterface[IO]

  private type Widget[Event] = SkijaWidget[IO, Float, String, String, Event, SkijaDownEvent[Float]]

  override def run(args: List[String]): IO[ExitCode] =
    skijaGlfwApp[IO, String, String, SkijaDownEvent[Float]](
      widget = main, 
      updateLoopExecutionContext = this.runtime.compute,
      drawLoopExecutionContext = MainThread,
      updateErrorAsExitCode = error => IO.println(error).as(ExitCode.Error),
      runEitherTError = [V] => (value : EitherT[IO, String, V]) =>
        value.value.flatMap:
          case Left(error) =>
            IO.raiseError(new Exception(error))
          case Right(value) =>
            IO.pure(value),
      createGlfwCallbacks = eventOfferingCallbacks,
      settings = WindowCreationSettings(
        title = "Gui4s window",
        size = Rect(620f, 480f),
        visible = true,
        resizeable = true,
        debugContext = true
      )
    )

  def eventCatcher[Event]: EventCatcherWithRect[Widget[Event], SkijaUpdate[IO, String, Float, Event, Boolean], Float, SkijaDownEvent[Float]] = eventCatcherWithWidgetsRect(
    markEventHandled,
    getCoordinates,
  )

  def mouseTracker[Event](name : String) : WithContext[Widget[Event], Option[Point2d[Float]]] =
    rememberLastEventOfTheType[Widget, SkijaUpdate[IO, String, Float, *, Boolean], Path => IO[Unit], Float, Event, SkijaDownEvent[Float], Point2d[Float]](
      eventCatcherWithRect = eventCatcher,
      statefulWidget = transitiveStatefulWidget,
      mapUpdate = [A, B] => f => mapEvents(f),
      mapEvent = library.mapEvent2([T, A, B] => f => mapEvents(f)),
      name = name,
      catchEvent =
        (path, rect, event) =>
          event match
            case SkijaDownEvent.MouseMove(newPosition) =>
              raiseEvents[IO, String, Float, Point2d[Float]](List(newPosition)).as(false)
            case _ => false.pure[SkijaUpdateT[IO, String, Float, Point2d[Float]]]
    )
  end mouseTracker

  def mmapEvent : MapEvent[Widget] =
    library.mapEvent2([T, A, B] => (f : A => B) => mapEvents(f))

  extension[Event](value : Widget[Event])
    def mapEvent[NewEvent](f : Event => NewEvent) : Widget[NewEvent] =
      mmapEvent.mapEvent(value)(f)
    end mapEvent
  end extension

  def clickHandler[Event](currentMousePosition : IO[Point2d[Float]]): ClickHandler[Widget[Event], SkijaUpdate[IO, String, Float, Event, Boolean], Unit] =
    makeClickHandler(
      eventCatcherWithRect = eventCatcher,
      currentMousePosition = liftIOToSkijaUpdate(currentMousePosition),
    )(
      extractClickHandlerEvent
    )

  extension[Event](widget : Widget[Event])
    def onClick(currentMousePosition : IO[Point2d[Float]])(event : Event) : Widget[Event] =
      clickHandler(currentMousePosition)(widget)(
        (_, _) =>
          raiseEvents[IO, String, Float, Event](List(event)).as(true)
      )
    end onClick
  end extension
  
  def statefulWidget: StatefulWidget[Widget, Path => IO[Unit]] = skijaStatefulWidget(
    (value: Any, path: Path) => "Error in stateful typechecking at " + path.toString + " with value [" + value.toString + "]",
  )

  def transitiveStatefulWidget: TransitiveStatefulWidget[Widget, Path => IO[Unit]] = TransitiveStatefulWidgetFromStatefulWidget(statefulWidget)

  type TextWidget[Widget[_]] = [Event] => (String, SkijaTextStyle) => Widget[Event]

  def text[Monitor, Window, DownEvent](using backend : SkijaBackend[IO, Monitor, Window, DownEvent]) : TextWidget[Widget] =
    [Event] => (text: String, style: SkijaTextStyle) =>
      skijaText(ffi, skijaSizeText(ffi, backend.globalShaper, backend.globalTextCache), text, style)
  end text

  def launchedEffect[Event, Key : Typeable](supervisor : Supervisor[IO]) : LaunchedEffectWidget[Widget[Event], Key, Path => IO[Unit]] =
    val lew : LaunchedEffectWidget[Widget[Event], Key, Path => SkijaRecomposition[IO]] = library.launchedEffect(
      [T] => (path : Path) => raiseError("Key has changed type at " + path.toString),
      SkijaRecomposition.lift[IO, Nothing](
        IO.raiseError(Exception("Key changed the type"))
      )
    )
    (name, child, key, task) =>
      lew(name, child, key, path =>
        SkijaRecomposition.lift(
          supervisor.supervise(task(path))
        )
      )

  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf", "org.wartremover.warts.Any"))
  given destructableIsTypeable[T : Typeable] : Typeable[(T, IO[Unit])] = {
    case (a: T, io: IO[t]) => Some[(T, IO[Unit])]((a, io.as(()))).map(_.asInstanceOf)
    case _ => None
  }

  // TODO refactor me
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def resource[Window, Monitor, Event](supervisor : Supervisor[IO], backend : SkijaBackend[IO, Monitor, Window, SkijaDownEvent[Float]]) : ResourceWidget[Widget[Event], IO] =
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
                      case Some(event) => raiseEvents[IO, String, Float, Either[(Value, IO[Unit]), Event]](List(Left(event))).as(true)
                      case _ => false.pure[SkijaUpdateT[IO, String, Float, Either[(Value, IO[Unit]), Event]]]
                  case _ => false.pure[SkijaUpdateT[IO, String, Float, Either[(Value, IO[Unit]), Event]]]
              ),
              (),
              path => resource.flatMap(value => backend.raiseEvent(SkijaDownEvent.TaskRaisedEvent(path, value)))
            )
        )
  end resource

  def leaf[Marker, Event](marker : Marker) : Widget[Event] =
    leafWidget[
      Marker,
      SkijaUpdateT[IO, String, Float, Event],
      SkijaPlaceT[IO, Float, String],
      SkijaDraw[IO, OglGlfwWindow],
      SkijaRecomposition[IO],
      SkijaDownEvent[Float]
    ](
      new Sized(
        marker,
        0f,
        0f
      ).pure[SkijaPlaceInnerT[IO, Float, String]],
      ReaderT.pure[IO, SkijaDrawState[IO, OglGlfwWindow], Unit](()),
      SkijaRecomposition.empty[IO]
    ).map(a => a)
  end leaf

  def main[Monitor](using SkijaBackend[IO, Monitor, OglGlfwWindow, SkijaDownEvent[Float]]) : Widget[ApplicationRequest] =
    grid((0 until 10).toList)
  end main

  def layout[Event](
                      children : List[Widget[Event]],
                      axis : Axis,
                      mainAxisStrategy : MainAxisPlacement[SkijaPlaceInnerT[IO, Float, String], Float],
                      additionalAxisStrategy : AdditionalAxisPlacement[SkijaPlaceInnerT[IO, Float, String], Float],
                    ) : Widget[Event] =
    skijaLayout(
      children,
      axis,
      mainAxisStrategy,
      additionalAxisStrategy,
      (draw, meta) => drawAt[IO, OglGlfwWindow](summon, draw, meta.x, meta.y),
      [T] => (update, meta) => addCoordinates[IO, String, Float, Event](meta.point) *> update <* addCoordinates[IO, String, Float, Event](-meta.point),
      isEventHandled[IO, String, Float, Event]
    )
  end layout

  def app[Monitor](numbers : List[Int])(
    using backend : SkijaBackend[IO, Monitor, OglGlfwWindow, SkijaDownEvent[Float]]
  ) : Widget[ApplicationRequest] =
    layout(
      axis = Axis.Vertical,
      mainAxisStrategy = MainAxisPlacement.Begin(0f),
      additionalAxisStrategy = AdditionalAxisPlacement.Center(ENErrors.withCenterStrategy),
      children = numbers.map:
        lineNumber =>
          statefulWidget[Int, ApplicationRequest, Unit](
            name = "line-" + lineNumber.toString,
            initialState = 0,
            eventHandler = (state, _) => EventReaction(state + 1, Nil, Nil),
            body = state =>
              text[Monitor, OglGlfwWindow, SkijaDownEvent[Float]](
                "# " + lineNumber.toString + " : " + state.toString,
                SkijaTextStyle(new Font(Typeface.makeDefault(), 24), new Paint().setColor(0xFF8484A4))
              ).onClick(backend.mousePosition)(())
          ),
    )
  end app

  def grid[
    Monitor, 
    Event
  ](
      numbers : List[Int]
  )(using SkijaBackend[IO, Monitor, OglGlfwWindow, SkijaDownEvent[Float]]) : Widget[Event] =
    layout(
      axis = Axis.Vertical,
      mainAxisStrategy = MainAxisPlacement.SpaceBetween(ENErrors.withSpaceBetweenStrategy),
      additionalAxisStrategy = AdditionalAxisPlacement.Begin,
      children =
        numbers.map:
          lineIndex =>
            layout(
              axis = Axis.Horizontal,
              mainAxisStrategy = MainAxisPlacement.SpaceBetween(ENErrors.withSpaceBetweenStrategy),
              additionalAxisStrategy = AdditionalAxisPlacement.Begin,
              children =
                numbers.map:
                  lineJindex =>
                    text[Monitor, OglGlfwWindow, SkijaDownEvent[Float]](
                      lineIndex.toString + ":" + lineJindex.toString,
                      SkijaTextStyle(new Font(Typeface.makeDefault(), 28), new Paint().setColor(0xFF8484A4))
                    )
            )
    )
end SkijaAppExample

@experimental
def extractClickHandlerEvent[MeasurementUnit](downEvent : SkijaDownEvent[MeasurementUnit]) : Option[Unit] =
  downEvent match
    case SkijaDownEvent.MouseClick(_, action, _) if action == Press =>
      Some(()) // TODO ClickHandlerDownEvent(button, action, mods))
    case _ => None
end extractClickHandlerEvent
