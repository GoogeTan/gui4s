package me.katze.gui4s.example
package examples

import api.*
import api.effects.SkijaDownEvent.catchExternalEvent
import api.effects.{*, given}
import api.widget.*
import app.skijaGlfwApp
import skija.SkijaBackend

import catnip.ForeighFunctionInterface
import catnip.effect.SyncForeighFunctionInterface
import catnip.syntax.all.{*, given}
import cats.*
import cats.data.{EitherT, NonEmptyList}
import cats.effect.std.{Dispatcher, Supervisor}
import cats.effect.{ExitCode, IO, IOApp, Resource}
import cats.syntax.all.*
import io.github.humbleui.skija.*
import io.github.humbleui.skija.shaper.Shaper
import me.katze.gui4s
import me.katze.gui4s.example
import me.katze.gui4s.geometry.*
import me.katze.gui4s.glfw.{OglGlfwWindow, WindowCreationSettings}
import me.katze.gui4s.layout.Sized
import me.katze.gui4s.skija.*
import me.katze.gui4s.widget.handle.HandlesEventF
import me.katze.gui4s.widget.library.{Widget, *}
import me.katze.gui4s.widget.library.decorator.*
import me.katze.gui4s.widget.{Path, StatefulState, library}
import org.http4s.Uri
import org.http4s.ember.client.EmberClientBuilder
import scalacache.caffeine.CaffeineCache

import scala.reflect.Typeable

object ImageExample extends IOApp with ExampleApp:
  given ffi: ForeighFunctionInterface[IO] = SyncForeighFunctionInterface[IO]

  type UpdateError = String
  type PlaceError = String

  override type Update[Event, Value] = SkijaUpdate[IO, UpdateEffectState[Point3d[Float], SkijaClip], UpdateError, Event, Value]

  type OuterPlace[Value] = SkijaOuterPlace[IO, Rect[Float], PlaceError, Value]
  type InnerPlace[Value] = Sized[Float, Value]

  override type Place = [Value] =>> OuterPlace[InnerPlace[Value]]

  override type Draw = SkijaDraw[IO]
  override type RecompositionReaction = SkijaRecomposition[IO]
  override type DownEvent = SkijaDownEvent[Float]

  type PreInit = (dispatcher: Dispatcher[IO], globalSupervisor: Supervisor[IO], shaper: Shaper, globalTextCache: TextCache[IO])

  def preInit(backend: SkijaBackend[IO, Long, OglGlfwWindow, SkijaDownEvent[Float]]): Resource[IO, PreInit] =
    for
      dispatcher <- Dispatcher.sequential[IO]
      supervisor <- Supervisor[IO]
      shaper <- backend.skija.createShaper
      cache: TextCache[IO] <- Resource.eval(CaffeineCache[IO, (String, SkijaTextStyle, Option[Float]), Sized[Float, SkijaPlacedText]]).map(scalacacheCache)
    yield (dispatcher, supervisor, shaper, cache)
  end preInit

  override def run(args: List[String]): IO[ExitCode] =
    skijaGlfwApp[
      IO,
      UpdateC[SkijaApplicationRequest],
      Place,
      Draw,
      RecompositionReaction,
      DownEvent,
      PreInit
    ](
      preInit = preInit,
      main = main,
      updateLoopExecutionContext = this.runtime.compute,
      drawLoopExecutionContext = MainThread,
      settings = WindowCreationSettings(
        title = "Gui4s image widget example",
        size = Rect(620f, 480f),
        visible = true,
        resizeable = true,
        debugContext = true
      ),
      ffi = ffi,
      callbacks = sink => SkijaDownEvent.eventOfferingCallbacks(sink.offer),
      runUpdate = SkijaUpdate.handleApplicationRequests[IO, Point3d[Float], SkijaClip, String](error => IO.println(error).as(ExitCode.Error)),
      runPlace = backend => SkijaPlace.run[IO, Rect[Float], Float, PlaceError](backend.windowBounds).andThen[EitherT[IO, Throwable, *]](eitherTMapError[IO, String, Throwable](new Exception(_))).andThen(runEitherT[IO, Throwable]),
      runDraw = (draw, backend) => backend.drawFrame(ffi, (clear[IO] |+| draw).run),
      runRecomposition = SkijaRecomposition.run[IO]
    )
  end run

  def main(preInit : PreInit, backend : SkijaBackend[IO, Long, OglGlfwWindow, DownEvent]) : Widget[SkijaApplicationRequest] =
    def updateDecorator[Event]: UpdateDecorator[UpdateC[Event], OuterPlace, InnerPlace[PlacedWidget[Event]], DownEvent] =
      updateDecoratorWithRect[UpdateC[Event], OuterPlace, InnerPlace, Draw, RecompositionReaction, DownEvent]
    end updateDecorator

    def eventCatcher[Event]: EventCatcherWithRect[
      Widget[Event],
      Update[Event, Boolean],
      InnerPlace[PlacedWidget[Event]],
      DownEvent,
    ] = eventCatcherWithRect[PlacedWidget[Event], UpdateC[Event], OuterPlace, InnerPlace, DownEvent](
      updateDecorator[Event],
      SkijaUpdate.markEventHandled[IO, Point3d[Float], SkijaClip, UpdateError, Event],
      widgetAsFree,
      widgetHandlesEvent[UpdateC[Event], Place, Draw, RecompositionReaction, DownEvent]
    )

    extension[Event](value : Widget[Event])
      def mapEvent[NewEvent](f : Event => NewEvent) : Widget[NewEvent] =
        library.decorator.mapEvent[Update, Place, Draw, RecompositionReaction, DownEvent]([T, A, B] => (f : A => B) => SkijaUpdate.mapEvents(f))(value)(f)
      end mapEvent

      def clip(path : Rect[Float] => SkijaClip) : Widget[Event] =
        clipWidget[
          UpdateC[Event],
          OuterPlace,
          InnerPlace,
          Draw,
          RecompositionReaction,
          DownEvent,
          SkijaClip
        ](
          [T] => (a, b) => SkijaUpdate.withClip[IO, Point3d[Float], SkijaClip, UpdateError, Event, T](a, b, SkijaClip.skijaPathAt),
          SkijaClip.clipToPath[IO](ffi, _ : SkijaClip, _ : SkijaDraw[IO]),
          place => path(place.size),
        )(value)
      end clip

      def gapPadding(paddings : Paddings[Float]) : Widget[Event]=
        gapPaddingWidget[
          UpdateC[Event],
          OuterPlace,
          InnerPlace,
          Draw,
          RecompositionReaction,
          DownEvent,
          Paddings[Float],
        ](
          paddings => [T] => place =>
            SkijaOuterPlace.withBounds[IO, Rect[Float], PlaceError, InnerPlace[T]](place, _.cut(paddings.horizontalLength, paddings.verticalLength, _ - _)),
          paddings => update => (path, event) =>
            SkijaUpdate.withCoordinates[IO, Point3d[Float], SkijaClip, UpdateError, Event, Widget[Event]](update(path, event))(_ + new Point3d(paddings.topLeftCornerShift)),
          paddings => draw => drawAt(ffi, draw.value, paddings.left, paddings.top),
        )(paddings)(value)
    end extension


    def statefulWidget : StatefulWidget[Widget, Update, [State] =>> State => RecompositionReaction] =
      new StatefulWidget[Widget, Update,  [State] =>> State => RecompositionReaction]:
        override def apply[State: Typeable, Event, ChildEvent](
                                                                name: PlaceError,
                                                                initialState: State,
                                                                eventHandler: HandlesEventF[State, NonEmptyList[ChildEvent], UpdateC[Event]],
                                                                body: State => Widget[ChildEvent]
                                                              ): Widget[Event] =
          apply(name, initialState, eventHandler, body, _ => SkijaRecomposition.empty[IO])
        end apply

        override def apply[State: Typeable, Event, ChildEvent](
                                                                name: PlaceError,
                                                                initialState: State,
                                                                eventHandler: HandlesEventF[State, NonEmptyList[ChildEvent], UpdateC[Event]],
                                                                body: State => Widget[ChildEvent],
                                                                destructor: State => SkijaRecomposition[IO]
                                                              ): Widget[Event] =
          library.stateful[
            UpdateC[Event],
            UpdateC[ChildEvent],
            Place,
            Draw,
            RecompositionReaction,
            DownEvent,
            State,
            ChildEvent
          ](
            widgetsAreMergeable = widgetsAreMergable[UpdateC[ChildEvent], OuterPlace, InnerPlace, Draw, RecompositionReaction, DownEvent],
            typeCheckState = SkijaPlace.typecheck[IO, Rect[Float], Float, String, StatefulState[State]]((value : Any, path : Path) => "Error in stateful typechecking at " + path.toString + " with value [" + value.toString + "]"),
            liftUpdate = SkijaUpdate.catchEvents[IO, UpdateEffectState[Point3d[Float], SkijaClip], UpdateError, ChildEvent, Event]
          )(
            name = name,
            initialState = initialState,
            handleEvent = eventHandler,
            render = body,
            destructor = destructor
          )
        end apply
      end new
    end statefulWidget

    def transitiveStatefulWidget: TransitiveStatefulWidget[Widget, Update] =
      TransitiveStatefulWidgetFromStatefulWidget[Widget, Update, [Value] =>> Value => SkijaRecomposition[IO]](
        statefulWidget, [Event] => events => SkijaUpdate.raiseEvents[IO, UpdateEffectState[Point3d[Float], SkijaClip], String, Event](events)
      )

    def text[Event](text : String, style : SkijaTextStyle) : Widget[Event] =
        me.katze.gui4s.widget.library.text[
          UpdateC[Event],
          Place,
          SkijaDraw[IO],
          RecompositionReaction,
          DownEvent,
          SkijaPlacedText
        ](
          SkijaPlace.sizeText[IO, Rect[Float], PlaceError](ffi, preInit.shaper, preInit.globalTextCache, _.width.some)(text, style),
          drawText(ffi, _),
          Monoid[RecompositionReaction].empty,
        )
    end text

    def launchedEffect[Event, Key : Typeable](supervisor : Supervisor[IO]) : LaunchedEffectWidget[Widget[Event], Key, Path => IO[Unit]] =
      val lew : LaunchedEffectWidget[
        Widget[Event],
        Key,
        Path => RecompositionReaction
      ] = library.launchedEffect(
        [T] => (path : Path, value : Any) => SkijaOuterPlace.raiseError("Key has changed type at " + path.toString + " value found " + value.toString),
        (valueFound : Any) => SkijaRecomposition.lift[IO, Nothing](
          IO.raiseError(Exception("Key changed the type: " + valueFound.toString))
        ),
      )
      (name, child, key, task) =>
        lew(name, child, key, path =>
          SkijaRecomposition.lift(
            supervisor.supervise(task(path))
          )
        )
    end launchedEffect

    @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf", "org.wartremover.warts.Any"))
    given Typeable[IO[Unit]] = a => a match
      case b: IO[t] => Some(b.as(()).asInstanceOf[IO[Unit] & a.type])
      case _ => None

    def launchedEvent[Event : Typeable, Key : Typeable](supervisor: Supervisor[IO]) : LaunchedEffectWidget[Widget[Event], Key, IO[Event]] =
      library.launchedEvent[
        IO,
        Widget[Event],
        Key,
        Update,
        Sized[Float, PlacedWidget[Event]],
        SkijaDownEvent[Float],
        Event
      ](
        launchedEffectWidget = launchedEffect(supervisor),
        eventCatcher = eventCatcher,
        pushEvent = (path, event) =>  backend.raiseEvent(SkijaDownEvent.ExternalEventForWidget(path, event)),
        catchEvent = (path, event) =>
          catchExternalEvent[Event, Float, String](path, event, (valueFound : Any) => "Event type mismatch in launched event at " + path + " with value found: " + valueFound.toString) match
            case None => false.pure[UpdateC[Event]]
            case Some(Right(event)) =>
              SkijaUpdate.raiseEvents[IO, UpdateEffectState[Point3d[Float], SkijaClip], String, Event](List(event)).as(true)
            case Some(Left(error)) =>
              SkijaUpdate.raiseError[IO, UpdateEffectState[Point3d[Float], SkijaClip], String, Event, Boolean](error)
      )
    end launchedEvent

    def resource[Event](supervisor : Supervisor[IO]) : ResourceWidget[Widget[Event], IO] =
      resourceWidget[
        Widget,
        Update,
        IO,
        Event
      ](
        transitiveStatefulWidget = transitiveStatefulWidget,
        launchedEffect =
          [TaskEvent : Typeable] => (name, child, task) =>
              launchedEvent[Either[TaskEvent, Event], Unit](supervisor)(
                name,
                child.mapEvent(Right(_)),
                (),
                task.map(Left(_))
              ),
        doubleAllocError = [T] => (path : Path) => SkijaUpdate.raiseError("Double resource alloc at " + path.toString)
      )
    end resource

    def resourceInit[Event, Value : Typeable](name : String, supervisor : Supervisor[IO], init : IO[Value]) : WithContext[Widget[Event], Option[Value]] =
      resource(supervisor)(name, init.map(value => (value, IO.unit)))
    end resourceInit

    def initWidget[Event, Value](
                                  name : String,
                                  imageSource : IO[Value],
                                  imageWidget : Value => Widget[Event],
                                  placeholder : Widget[Event]
                                ) : Widget[Event] =
      resourceInit(
        name,
        preInit.globalSupervisor,
        imageSource
      ) {
        case Some(image) => imageWidget(image)
        case None => placeholder
      }
    end initWidget

    def downloadImage(uri: String): IO[Image] =
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

    def image[Event](
        image: Image,
    ): Widget[Event] =
      drawOnlyWidget[
        UpdateC[Event],
        Place,
        Draw,
        RecompositionReaction,
        DownEvent,
      ](
        Sized(drawImage(ffi, image), Rect(image.getWidth.toFloat, image.getHeight.toFloat)).pure[OuterPlace],
        Monoid[RecompositionReaction].empty,
      )
    end image

    initWidget(
      name = "image",
      imageSource = downloadImage("https://i.pinimg.com/1200x/1b/6e/8c/1b6e8c66f6d302c0c0156104a52a32be.jpg"),
      imageWidget = image,
      placeholder = text("Wait.", SkijaTextStyle(new Font(Typeface.makeDefault(), 28), new Paint().setColor(0xFF8484A4)))
    ).clip(
      SkijaClip.Shapes.round
    ).gapPadding(
      Paddings(10f, 10f, 10f, 10f)
    )
  end main
end ImageExample