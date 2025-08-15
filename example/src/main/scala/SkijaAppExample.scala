package me.katze.gui4s.example

import api.*
import api.effects.{*, given}
import app.{SkijaWidget, skijaGlfwApp}
import place.*
import skija.SkijaBackend
import update.ApplicationRequest

import catnip.ForeighFunctionInterface
import catnip.cats.effect.SyncForeighFunctionInterface
import catnip.syntax.all.{*, given}
import cats.*
import cats.data.*
import cats.effect.std.{Dispatcher, Supervisor}
import cats.effect.{ExitCode, IO, IOApp, Resource}
import cats.syntax.all.*
import io.github.humbleui.skija.*
import io.github.humbleui.skija.shaper.Shaper
import me.katze.gui4s
import me.katze.gui4s.example
import me.katze.gui4s.example.api.widget.{skijaLayout, skijaStateful, skijaText}
import me.katze.gui4s.geometry.{Axis, Point2d, Point3d, Rect, given}
import me.katze.gui4s.glfw.KeyAction.Press
import me.katze.gui4s.glfw.{KeyAction, KeyModes, OglGlfwWindow, WindowCreationSettings}
import me.katze.gui4s.layout.Sized
import me.katze.gui4s.layout.rowcolumn.{AdditionalAxisPlacement, MainAxisPlacement}
import me.katze.gui4s.skija.*
import me.katze.gui4s.widget.library.{*, given}
import me.katze.gui4s.widget.{Path, library}
import org.http4s.Uri
import org.http4s.ember.client.EmberClientBuilder
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
type ResourceWidget[Widget, F[_]] = [T : Typeable] => (name : String, resource : F[(T, F[Unit])]) => WithContext[Widget, Option[T]]

@experimental
object SkijaAppExample extends IOApp:
  given ElementPlacementInInfiniteContainerAttemptError[String] = ENErrors
  given ffi : ForeighFunctionInterface[IO] = SyncForeighFunctionInterface[IO]

  private type Widget[Event] = SkijaWidget[IO, Float, SkijaClip, String, String, Event, SkijaDownEvent[Float]]

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
    skijaGlfwApp[IO, SkijaClip, String, String, SkijaDownEvent[Float], PreInit](
      preInit = preInit,
      widget = main(_),
      updateLoopExecutionContext = this.runtime.compute,
      drawLoopExecutionContext = MainThread,
      updateErrorAsExitCode = error => IO.println(error).as(ExitCode.Error),
      runEitherTError =
        new ~>[EitherT[IO, String, *], IO]:
          override def apply[A](fa: EitherT[IO, String, A]): IO[A] =
            fa.value.flatMap:
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

    def eventCatcher[Event]: EventCatcherWithRect[Widget[Event], SkijaUpdate[IO, Float, SkijaClip, String, Event, Boolean], Float, SkijaDownEvent[Float]] = eventCatcherWithWidgetsRect(
      SkijaUpdate.markEventHandled,
      SkijaUpdate.getCoordinates,
    )

    def mouseTracker[Event](name : String) : WithContext[Widget[Event], Option[Point2d[Float]]] =
      rememberLastEventOfTheType[Widget, SkijaUpdate[IO, Float, SkijaClip, String, *, *], Float, Event, SkijaDownEvent[Float], Point2d[Float], Boolean](
        eventCatcherWithRect = eventCatcher,
        statefulWidget = transitiveStatefulWidget,
        mapUpdate = [A, B] => f => SkijaUpdate.mapEvents(f),
        mapEvent = mmapEvent,
        name = name,
        catchEvent =
          (path, rect, event) =>
            event match
              case SkijaDownEvent.MouseMove(newPosition) =>
                SkijaUpdate.raiseEvents[IO, Float, SkijaClip, String, Point2d[Float]](List(newPosition)).as(false)
              case _ => false.pure[SkijaUpdateT[IO, Float, SkijaClip, String, Point2d[Float]]]
      )
    end mouseTracker

    def mmapEvent : MapEvent[Widget] =
      library.mapEvent([T, A, B] => (f : A => B) => SkijaUpdate.mapEvents(f))

    extension[Event](value : Widget[Event])
      def mapEvent[NewEvent](f : Event => NewEvent) : Widget[NewEvent] =
        mmapEvent.apply(value)(f)
      end mapEvent

      def gapPadding(paddings: Paddings[Float]) : Widget[Event] =
        gapPaddingWidget[
          SkijaUpdateT[IO, Float, SkijaClip, String, Event],
          SkijaOuterPlaceT[IO, Float, String],
          Sized[Float, *],
          SkijaDraw[IO],
          SkijaRecomposition[IO],
          SkijaDownEvent[Float],
          Paddings[Float],
        ](
          (paddings : Paddings[Float]) =>
            [T] => (place : SkijaPlace[IO, Float, String, T]) => SkijaOuterPlace.withBounds[IO, Float, String, Sized[Float, T]](place, _.cut(paddings.horizontalLength, paddings.verticalLength)),
          (paddings : Paddings[Float]) =>
              (update : WidgetHandlesEvent[SkijaDownEvent[Float], SkijaUpdate[IO, Float, SkijaClip, String, Event, Widget[Event]]]) =>
                (path : Path, event : SkijaDownEvent[Float]) =>
                  SkijaUpdate.withCoordinates[IO, Float, SkijaClip, String, Event, Widget[Event]](update(path, event))(_ + new Point3d(paddings.topLeftCornerShift)),
          (paddings : Paddings[Float]) => draw => drawAt(ffi, draw.value, paddings.left, paddings.top),
        )(paddings)(value)
      end gapPadding

      def padding(padding: Paddings[Padding[Float]]) : Widget[Event] =
        paddingWidget[
          SkijaUpdateT[IO, Float, SkijaClip, String, Event],
          SkijaOuterPlaceT[IO, Float, String],
          SkijaPlaceT[IO, Float, String],
          SkijaDraw[IO],
          SkijaRecomposition[IO],
          SkijaDownEvent[Float],
          Float,
          String,
        ](
          gapPaddings => widget => widget.gapPadding(gapPaddings),
          layout[Event],
          "Infinite padding accured in a infinite size container"
        )(padding)(value)
      end padding

      def clip(path : Rect[Float] => SkijaClip) : Widget[Event] =
        clipWidget[
          SkijaUpdateT[IO, Float, SkijaClip, String, Event],
          SkijaOuterPlaceT[IO, Float, String],
          SkijaDraw[IO],
          SkijaRecomposition[IO],
          SkijaDownEvent[Float],
          Float,
          SkijaClip,
        ](
          [T] => (a, b) => SkijaUpdate.withClip[IO, Float, SkijaClip, String, Event, T](a, b, SkijaClip.skijaPathAt),
          SkijaClip.clipToPath[IO](ffi, _ : SkijaClip, _ : SkijaDraw[IO])
        )(value, path)
      end clip
    end extension

    def image[Event](image : Image) : Widget[Event] =
      drawOnlyWidget[
        SkijaUpdateT[IO, Float, SkijaClip, String, Event],
        SkijaPlaceT[IO, Float, String],
        SkijaDraw[IO],
        SkijaRecomposition[IO],
        SkijaDownEvent[Float],
      ](
        Sized(drawImage(ffi, image), Rect(image.getWidth.toFloat, image.getHeight.toFloat)).pure[SkijaOuterPlaceT[IO, Float, String]],
        SkijaRecomposition.empty[IO]
      )
    end image

    extension[Event](widget : Widget[Event])
      def onClick(event : Event) : Widget[Event] =
        makeClickHandler(
          eventCatcherWithRect = eventCatcher,
          currentMousePosition = SkijaUpdate.liftF(backend.mousePosition),
        )(
          extractClickHandlerEvent
        )(widget)(
          (_, _) =>
            SkijaUpdate.raiseEvents[IO, Float, SkijaClip, String, Event](List(event)).as(true)
        )
      end onClick
    end extension

    def statefulWidget: StatefulWidget[Widget, SkijaUpdate[IO, Float, SkijaClip, String, *, *], [Value] =>> Value => SkijaRecomposition[IO]] = skijaStateful(
      (value: Any, path: Path) => "Error in stateful typechecking at " + path.toString + " with value [" + value.toString + "]"
    )

    def transitiveStatefulWidget: TransitiveStatefulWidget[Widget, SkijaUpdate[IO, Float, SkijaClip, String, *, *]] =
      TransitiveStatefulWidgetFromStatefulWidget[Widget, SkijaUpdate[IO, Float, SkijaClip, String, *, *], [Value] =>> Value => SkijaRecomposition[IO]](
        statefulWidget, [Event] => events => SkijaUpdate.raiseEvents[IO, Float, SkijaClip, String, Event](events)
      )

    type TextWidget[Widget[_]] = [Event] => (String, SkijaTextStyle) => Widget[Event]

    def text : TextWidget[Widget] =
      [Event] => (text: String, style: SkijaTextStyle) =>
        skijaText[
          IO,
          SkijaUpdateT[IO, Float, SkijaClip, String, Event],
          SkijaPlaceT[IO, Float, String],
          SkijaRecomposition[IO],
          SkijaDownEvent[Float],
        ](ffi, SkijaPlace.sizeText(ffi, preInit.shaper, preInit.globalTextCache), text, style)
    end text

    def launchedEffect[Event, Key : Typeable](supervisor : Supervisor[IO]) : LaunchedEffectWidget[Widget[Event], Key, Path => IO[Unit]] =
      val lew : LaunchedEffectWidget[Widget[Event], Key, Path => SkijaRecomposition[IO]] = library.launchedEffect(
        [T] => (path : Path, value : Any) => SkijaOuterPlace.raiseError("Key has changed type at " + path.toString + " value found " + value.toString),
        (valueFound : Any) => SkijaRecomposition.lift[IO, Nothing](
          IO.raiseError(Exception("Key changed the type: " + valueFound.toString))
        )
      )
      (name, child, key, task) =>
        lew(name, child, key, path =>
          SkijaRecomposition.lift(
            supervisor.supervise(task(path))
          )
        )

    @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf", "org.wartremover.warts.Any"))
    given destructableIsTypeable[T : Typeable] : Typeable[(T, IO[Unit])] = x => x match {
      case (a: T, io: IO[t]) =>
        Some[(T, IO[Unit])]((a, io.as(()))).map(_.asInstanceOf[x.type & (T, IO[Unit])])
      case _ => None
    }

    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    def catchTaskRaisedEvent[Event, Value : Typeable](value : Any, expectedPath : Path) : SkijaUpdate[IO, Float, SkijaClip, String, Either[(Value, IO[Unit]), Event], Boolean] =
      value match
        case SkijaDownEvent.TaskRaisedEvent(taskPath, value: Any) if expectedPath == taskPath =>
          destructableIsTypeable[Value].unapply(value) match
            case Some(event) =>
              SkijaUpdate.raiseEvents[IO, Float, SkijaClip, String, Either[(Value, IO[Unit]), Event]](List(Left(event))).as(true)
            case _ =>
              false.pure[SkijaUpdateT[IO, Float, SkijaClip, String, Either[(Value, IO[Unit]), Event]]]
          end match
        case _ => false.pure[SkijaUpdateT[IO, Float, SkijaClip, String, Either[(Value, IO[Unit]), Event]]]
      end match
    end catchTaskRaisedEvent

    // TODO refactor me
    def resource[Event](supervisor : Supervisor[IO]) : ResourceWidget[Widget[Event], IO] =
      [Value : Typeable] => (name : String, resource : IO[(Value, IO[Unit])]) =>
        (widget : Option[Value] => Widget[Event]) =>
          transitiveStatefulWidget[
            Option[(Value, IO[Unit])],
            Event,
            (Value, IO[Unit])
          ](
            name = name,
            initialState = None,
            eventHandler = {
              case (None, _, NonEmptyList(event, Nil)) =>
                Some(event).pure[SkijaUpdateT[IO, Float, SkijaClip, String, Event]]
              case _ => SkijaUpdate.raiseError("Resource was allocated twice")
            },
            body = state =>
              launchedEffect[Either[(Value, IO[Unit]), Event], Unit](supervisor)(
                "effect_launcher",
                eventCatcher(
                  widget(state.map(_._1)).mapEvent(Right(_))
                )(
                  (path, _, event) =>
                    catchTaskRaisedEvent(event, path)
                ),
                (),
                path => resource.flatMap(value => backend.raiseEvent(SkijaDownEvent.TaskRaisedEvent(path, value)))
              )
          )
    end resource

    def resourceInit[Event, Value : Typeable](name : String, supervisor : Supervisor[IO], init : IO[Value]) : WithContext[Widget[Event], Option[Value]] =
      resource(supervisor)(name, init.map(value => (value, IO.unit)))
    end resourceInit

    def downloadImage(uri : String): IO[Image] =
      EmberClientBuilder
        .default[IO]
        .build
        .use(
          client =>
            IO.fromEither(
              Uri.fromString(uri)
            ).flatMap(
              client.expect[Array[Byte]]
            ).map(Image.makeDeferredFromEncodedBytes)
        )
    end downloadImage

    def imageUrl[Event](name : String, uri : String, placeholder : => Widget[Event]) : Widget[Event] =
      resourceInit(name, preInit.globalSupervisor, downloadImage(uri)) {
        case Some(imageData) => image(imageData)
        case None => placeholder
      }
    end imageUrl

    def leaf[Marker, Event](marker : Marker) : Widget[Event] =
      leafWidget[
        Marker,
        SkijaUpdateT[IO, Float, SkijaClip, String, Event],
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

    def layout[Event] : LinearLayout[Widget[Event], SkijaOuterPlaceT[IO, Float, String], Float, Axis] =
      (
        children : List[Widget[Event]],
        axis : Axis,
        mainAxisStrategy : MainAxisPlacement[SkijaOuterPlaceT[IO, Float, String], Float],
        additionalAxisStrategy : AdditionalAxisPlacement[SkijaOuterPlaceT[IO, Float, String], Float],
      ) =>
      skijaLayout(
        children,
        axis,
        mainAxisStrategy,
        additionalAxisStrategy,
        (draw, point) => drawAt[IO](summon, draw, point.x, point.y),
        [T] => (update, point) => SkijaUpdate.withCoordinates(update)(_ => point),
        SkijaUpdate.isEventHandled[IO, Float, SkijaClip, String, Event]
      )
    end layout

    def app(numbers : List[Int]): Widget[ApplicationRequest] =
      layout(
        mainAxis = Axis.Vertical,
        mainAxisStrategy = MainAxisPlacement.Begin(0f),
        additionalAxisStrategy = AdditionalAxisPlacement.Center(ENErrors.withCenterStrategy),
        children = numbers.map:
          lineNumber =>
            statefulWidget[Int, ApplicationRequest, Unit](
              name = "line-" + lineNumber.toString,
              initialState = 0,
              eventHandler = (state, _, _) => (state + 1).pure[SkijaUpdateT[IO, Float, SkijaClip, String, ApplicationRequest]],
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
        mainAxis = Axis.Vertical,
        mainAxisStrategy = MainAxisPlacement.SpaceBetween(ENErrors.withSpaceBetweenStrategy),
        additionalAxisStrategy = AdditionalAxisPlacement.Begin,
        children =
          numbers.map:
            lineIndex =>
              layout(
                mainAxis = Axis.Horizontal,
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

    imageUrl(
      name = "image",
      uri = "https://i.pinimg.com/1200x/1b/6e/8c/1b6e8c66f6d302c0c0156104a52a32be.jpg",
      text("Wait.", SkijaTextStyle(new Font(Typeface.makeDefault(), 28), new Paint().setColor(0xFF8484A4)))
    ).clip(
      SkijaClip.Shapes.round
    ).gapPadding(
      Paddings(10f, 10f, 10f, 10f)
    )
  end main
end SkijaAppExample

@experimental
def extractClickHandlerEvent[MeasurementUnit](downEvent : SkijaDownEvent[MeasurementUnit]) : Option[Unit] =
  downEvent match
    case SkijaDownEvent.MouseClick(_, action, _) if action == Press =>
      Some(()) // TODO ClickHandlerDownEvent(button, action, mods))
    case _ => None
end extractClickHandlerEvent
