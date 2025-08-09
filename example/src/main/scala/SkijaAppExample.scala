package me.katze.gui4s.example

import api.exported.{*, given}
import app.{SkijaWidget, skijaGlfwApp}
import place.*
import skija.SkijaBackend
import update.ApplicationRequest

import catnip.ForeighFunctionInterface
import catnip.cats.effect.SyncForeighFunctionInterface
import catnip.syntax.all.given
import cats.data.*
import cats.effect.std.{Dispatcher, Supervisor}
import cats.effect.{ExitCode, IO, IOApp, Resource}
import cats.syntax.all.*
import io.github.humbleui.skija.shaper.Shaper
import io.github.humbleui.skija.{Canvas, Font, Paint, Typeface}
import me.katze.gui4s
import me.katze.gui4s.example
import me.katze.gui4s.geometry.{Axis, Point2d, Rect, given }
import me.katze.gui4s.glfw.KeyAction.Press
import me.katze.gui4s.glfw.{KeyAction, KeyModes, OglGlfwWindow, WindowCreationSettings}
import me.katze.gui4s.layout.rowcolumn.{AdditionalAxisPlacement, MainAxisPlacement}
import me.katze.gui4s.layout.{Sized, given}
import me.katze.gui4s.skija.*
import me.katze.gui4s.widget.library.{*, given}
import me.katze.gui4s.widget.{Path, library}
import scalacache.caffeine.CaffeineCache

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

  type PreInit = (dispatcher : Dispatcher[IO], globalSupervisor : Supervisor[IO], shaper : Shaper, globalTextCache : Cache)

  type Cache = CaffeineCache[IO, (String, SkijaTextStyle, Option[Float]), Sized[Float, SkijaPlacedText]]

  def preInit(backend : SkijaBackend[IO, Long, OglGlfwWindow, SkijaDownEvent[Float]]) : Resource[IO, PreInit] =
    for
      dispatcher <- Dispatcher.sequential[IO]
      supervisor <- Supervisor[IO]
      shaper <- backend.skija.createShaper
      cache : Cache <- Resource.eval(CaffeineCache[IO, (String, SkijaTextStyle, Option[Float]), Sized[Float, SkijaPlacedText]])
    yield (dispatcher, supervisor, shaper, cache)
  end preInit

  override def run(args: List[String]): IO[ExitCode] =
    skijaGlfwApp[IO, String, String, SkijaDownEvent[Float], PreInit](
      preInit = preInit,
      widget = main(_),
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
  end run

  def main(preInit : PreInit)(using backend : SkijaBackend[IO, Long, OglGlfwWindow, SkijaDownEvent[Float]]) : Widget[ApplicationRequest] =

    def eventCatcher[Event]: EventCatcherWithRect[Widget[Event], SkijaUpdate[IO, Float, String, Event, Boolean], Float, SkijaDownEvent[Float]] = eventCatcherWithWidgetsRect(
      SkijaUpdate.markEventHandled,
      SkijaUpdate.getCoordinates,
    )

    def mouseTracker[Event](name : String) : WithContext[Widget[Event], Option[Point2d[Float]]] =
      rememberLastEventOfTheType[Widget, SkijaUpdate[IO, Float, String, *, *], Float, Event, SkijaDownEvent[Float], Point2d[Float], Boolean](
        eventCatcherWithRect = eventCatcher,
        statefulWidget = transitiveStatefulWidget,
        mapUpdate = [A, B] => f => SkijaUpdate.mapEvents(f),
        mapEvent = mmapEvent,
        name = name,
        catchEvent =
          (path, rect, event) =>
            event match
              case SkijaDownEvent.MouseMove(newPosition) =>
                SkijaUpdate.raiseEvents[IO, Float, String, Point2d[Float]](List(newPosition)).as(false)
              case _ => false.pure[SkijaUpdateT[IO, Float, String, Point2d[Float]]]
      )
    end mouseTracker

    def mmapEvent : MapEvent[Widget] =
      library.mapEvent([T, A, B] => (f : A => B) => SkijaUpdate.mapEvents(f))

    extension[Event](value : Widget[Event])
      def mapEvent[NewEvent](f : Event => NewEvent) : Widget[NewEvent] =
        mmapEvent.apply(value)(f)
      end mapEvent
    end extension

    extension[Event](widget : Widget[Event])
      def onClick(event : Event) : Widget[Event] =
        makeClickHandler(
          eventCatcherWithRect = eventCatcher,
          currentMousePosition = SkijaUpdate.liftF(backend.mousePosition),
        )(
          extractClickHandlerEvent
        )(widget)(
          (_, _) =>
            SkijaUpdate.raiseEvents[IO, Float, String, Event](List(event)).as(true)
        )
      end onClick
    end extension

    def statefulWidget: StatefulWidget[Widget, SkijaUpdate[IO, Float, String, *, *], [Value] =>> Value => SkijaRecomposition[IO]] = skijaStateful(
      (value: Any, path: Path) => "Error in stateful typechecking at " + path.toString + " with value [" + value.toString + "]"
    )

    def transitiveStatefulWidget: TransitiveStatefulWidget[Widget, SkijaUpdate[IO, Float, String, *, *]] =
      TransitiveStatefulWidgetFromStatefulWidget[Widget, SkijaUpdate[IO, Float, String, *, *], [Value] =>> Value => SkijaRecomposition[IO]](
        statefulWidget, [Event] => events => SkijaUpdate.raiseEvents[IO, Float, String, Event](events)
      )

    type TextWidget[Widget[_]] = [Event] => (String, SkijaTextStyle) => Widget[Event]

    def text : TextWidget[Widget] =
      [Event] => (text: String, style: SkijaTextStyle) =>
        skijaText(ffi, SkijaPlace.sizeText(ffi, preInit.shaper, preInit.globalTextCache), text, style)
    end text

    def launchedEffect[Event, Key : Typeable](supervisor : Supervisor[IO]) : LaunchedEffectWidget[Widget[Event], Key, Path => IO[Unit]] =
      val lew : LaunchedEffectWidget[Widget[Event], Key, Path => SkijaRecomposition[IO]] = library.launchedEffect(
        [T] => (path : Path) => SkijaOuterPlace.raiseError("Key has changed type at " + path.toString),
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

    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    def catchTaskRaisedEvent[Event, Value : Typeable](value : Any, expectedPath : Path) : SkijaUpdate[IO, Float, String, Either[(Value, IO[Unit]), Event], Boolean] =
      value match
        case SkijaDownEvent.TaskRaisedEvent(taskPath, value: Any) if expectedPath == taskPath =>
          destructableIsTypeable.unapply(value) match
            case Some(event) => SkijaUpdate.raiseEvents[IO, Float, String, Either[(Value, IO[Unit]), Event]](List(Left(event))).as(true)
            case _ => false.pure[SkijaUpdateT[IO, Float, String, Either[(Value, IO[Unit]), Event]]]
          end match
        case _ => false.pure[SkijaUpdateT[IO, Float, String, Either[(Value, IO[Unit]), Event]]]
      end match
    end catchTaskRaisedEvent

    // TODO refactor me
    def resource[Event](supervisor : Supervisor[IO]) : ResourceWidget[Widget[Event], IO] =
      [Value : Typeable] => (name : String, resource : IO[(Value, IO[Unit])]) =>
        (widget : Option[Value] => Widget[Event]) =>
          transitiveStatefulWidget[Option[(Value, IO[Unit])], Event, (Value, IO[Unit])](
            name = name,
            initialState = None,
            eventHandler = (state, _, events) =>
              (state, events) match
                case (None, NonEmptyList(event, Nil)) => Some(event).pure[SkijaUpdateT[IO, Float, String, Event]]
                case _ => SkijaUpdate.raiseError("Resource was allocated twice"),
            body = state =>
              launchedEffect[Either[(Value, IO[Unit]), Event], Unit](supervisor)(
                name + "_effect_launcher",
                eventCatcher(
                  widget(state.map(_._1)).mapEvent(Right(_))
                )(
                  (path, _, event) => catchTaskRaisedEvent(event, path)
                ),
                (),
                path => resource.flatMap(value => backend.raiseEvent(SkijaDownEvent.TaskRaisedEvent(path, value)))
              )
          )
    end resource

    def leaf[Marker, Event](marker : Marker) : Widget[Event] =
      leafWidget[
        Marker,
        SkijaUpdateT[IO, Float, String, Event],
        SkijaPlaceT[IO, Float, String],
        SkijaDraw[IO],
        SkijaRecomposition[IO],
        SkijaDownEvent[Float]
      ](
        new Sized(
          marker,
          0f,
          0f
        ).pure[SkijaOuterPlaceT[IO, Float, String]],
        ReaderT.pure[IO, Canvas, Unit](()),
        SkijaRecomposition.empty[IO]
      ).map(a => a)
    end leaf


    def layout[Event](
                        children : List[Widget[Event]],
                        axis : Axis,
                        mainAxisStrategy : MainAxisPlacement[SkijaOuterPlaceT[IO, Float, String], Float],
                        additionalAxisStrategy : AdditionalAxisPlacement[SkijaOuterPlaceT[IO, Float, String], Float],
                      ) : Widget[Event] =
      skijaLayout(
        children,
        axis,
        mainAxisStrategy,
        additionalAxisStrategy,
        (draw, meta) => drawAt[IO](summon, draw, meta.x, meta.y),
        [T] => (update, meta) => SkijaUpdate.withCoordinates(update)(_ => meta.point),
        SkijaUpdate.isEventHandled[IO, Float, String, Event]
      )
    end layout

    def app(numbers : List[Int]): Widget[ApplicationRequest] =
      layout(
        axis = Axis.Vertical,
        mainAxisStrategy = MainAxisPlacement.Begin(0f),
        additionalAxisStrategy = AdditionalAxisPlacement.Center(ENErrors.withCenterStrategy),
        children = numbers.map:
          lineNumber =>
            statefulWidget[Int, ApplicationRequest, Unit](
              name = "line-" + lineNumber.toString,
              initialState = 0,
              eventHandler = (state, _, _) => (state + 1).pure[SkijaUpdateT[IO, Float, String, ApplicationRequest]],
              body = state =>
                text(
                  "# " + lineNumber.toString + " : " + state.toString,
                  SkijaTextStyle(new Font(Typeface.makeDefault(), 24), new Paint().setColor(0xFF8484A4))
                ).onClick(())
            ),
      )
    end app

    def grid[Event](numbers : List[Int]) : Widget[Event] =
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
                      text(
                        lineIndex.toString + ":" + lineJindex.toString,
                        SkijaTextStyle(new Font(Typeface.makeDefault(), 28), new Paint().setColor(0xFF8484A4))
                      )
              )
      )
    end grid

    grid((0 until 10).toList)
  end main
end SkijaAppExample

@experimental
def extractClickHandlerEvent[MeasurementUnit](downEvent : SkijaDownEvent[MeasurementUnit]) : Option[Unit] =
  downEvent match
    case SkijaDownEvent.MouseClick(_, action, _) if action == Press =>
      Some(()) // TODO ClickHandlerDownEvent(button, action, mods))
    case _ => None
end extractClickHandlerEvent
