package me.katze.gui4s.example
package examples

import api.*
import api.effects.SkijaDownEvent.{catchExternalEvent, eventOfferingCallbacks}
import api.effects.{*, given}
import api.widget.*
import app.{SkijaPlacedWidget, SkijaWidget, skijaGlfwCatsApp}
import skija.SkijaBackend
import update.ApplicationRequest

import catnip.ForeighFunctionInterface
import catnip.cats.effect.SyncForeighFunctionInterface
import catnip.syntax.all.{*, given}
import cats.*
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
import me.katze.gui4s.widget.library.*
import me.katze.gui4s.widget.library.decorator.*
import me.katze.gui4s.widget.{Path, library}
import scalacache.caffeine.CaffeineCache

import scala.reflect.Typeable

object ImageExample extends IOApp:
  given ffi : ForeighFunctionInterface[IO] = SyncForeighFunctionInterface[IO]

  private type PlacedWidget[Event] = SkijaPlacedWidget[IO, Float, SkijaClip, String, String, Event, SkijaDownEvent[Float]]
  private type Widget[Event] = SkijaWidget[IO, Float, SkijaClip, String, String, Event, SkijaDownEvent[Float]]

  type PreInit = (dispatcher : Dispatcher[IO], globalSupervisor : Supervisor[IO], shaper : Shaper, globalTextCache : TextCache[IO])

  def preInit(backend : SkijaBackend[IO, Long, OglGlfwWindow, SkijaDownEvent[Float]]) : Resource[IO, PreInit] =
    for
      dispatcher <- Dispatcher.sequential[IO]
      supervisor <- Supervisor[IO]
      shaper <- backend.skija.createShaper
      cache : TextCache[IO] <- Resource.eval(CaffeineCache[IO, (String, SkijaTextStyle, Option[Float]), Sized[Float, SkijaPlacedText]]).map(scalacacheCache)
    yield (dispatcher, supervisor, shaper, cache)
  end preInit

  override def run(args: List[String]): IO[ExitCode] =
    skijaGlfwCatsApp(
      preInit = preInit,
      widget = main(_),
      updateLoopExecutionContext = this.runtime.compute,
      drawLoopExecutionContext = MainThread,
      updateErrorAsThrowable = (errorText : String) => new Exception(errorText),
      placeErrorAsThrowable = (errorText : String) => new Exception(errorText),
      createGlfwCallbacks = eventOfferingCallbacks,
      settings = WindowCreationSettings(
        title = "Gui4s image widget example",
        size = Rect(620f, 480f),
        visible = true,
        resizeable = true,
        debugContext = true
      )
    )
  end run

  def main(preInit : PreInit)(using backend : SkijaBackend[IO, Long, OglGlfwWindow, SkijaDownEvent[Float]]) : Widget[ApplicationRequest] =
    def eventCatcher[Event]: EventCatcherWithRect[
      Widget[Event],
      SkijaUpdate[IO, Float, SkijaClip, String, Event, Boolean],
      Sized[Float, PlacedWidget[Event]],
      SkijaDownEvent[Float]
    ] = eventCatcherWithRect(
      updateDecoratorWithRect,
      SkijaUpdate.markEventHandled,
      widgetAsFree,
      widgetHandlesEvent
    )

    extension[Event](value : Widget[Event])
      def mapEvent[NewEvent](f : Event => NewEvent) : Widget[NewEvent] =
        skijaMapEvent[
          IO, Float, SkijaClip, String, SkijaPlaceT[IO, Float, String], SkijaDraw[IO], SkijaRecomposition[IO], SkijaDownEvent[Float]
        ](value)(f)
      end mapEvent

      def clip(path : Rect[Float] => SkijaClip) : Widget[Event] =
        skijaClip[
          IO,
          String,
          SkijaOuterPlaceT[IO, Float, String],
          Sized[Float, *],
          SkijaRecomposition[IO],
          SkijaDownEvent[Float],
          Event
        ](ffi)(a => path(a.size))(value)
      end clip
    end extension

    def statefulWidget: StatefulWidget[Widget, SkijaUpdate[IO, Float, SkijaClip, String, *, *], [Value] =>> Value => SkijaRecomposition[IO]] = skijaStateful(
      (value: Any, path: Path) => "Error in stateful typechecking at " + path.toString + " with value [" + value.toString + "]"
    )

    def transitiveStatefulWidget: TransitiveStatefulWidget[Widget, SkijaUpdate[IO, Float, SkijaClip, String, *, *]] =
      TransitiveStatefulWidgetFromStatefulWidget[Widget, SkijaUpdate[IO, Float, SkijaClip, String, *, *], [Value] =>> Value => SkijaRecomposition[IO]](
        statefulWidget, [Event] => events => SkijaUpdate.raiseEvents[IO, Float, SkijaClip, String, Event](events)
      )

    def text[Event] : TextWidget[Widget[Event]] =
        skijaText(ffi, preInit.shaper, preInit.globalTextCache)
    end text

    def launchedEffect[Event, Key : Typeable](supervisor : Supervisor[IO]) : LaunchedEffectWidget[Widget[Event], Key, Path => IO[Unit]] =
      skijaLaunchedEffect(
        supervisor,
        [T] => (path : Path, value : Any) => SkijaOuterPlace.raiseError("Key has changed type at " + path.toString + " value found " + value.toString),
        (valueFound : Any) => SkijaRecomposition.lift[IO, Nothing](
          IO.raiseError(Exception("Key changed the type: " + valueFound.toString))
        ),
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
        SkijaUpdate[IO, Float, SkijaClip, String, *, *],
        Sized[Float, PlacedWidget[Event]],
        SkijaDownEvent[Float],
        Event
      ](
        launchedEffectWidget = launchedEffect(supervisor),
        eventCatcher = eventCatcher,
        pushEvent = (path, event) =>  backend.raiseEvent(SkijaDownEvent.ExternalEventForWidget(path, event)),
        catchEvent = (path, event) =>
          catchExternalEvent[Event, Float, String](path, event, (valueFound : Any) => "Event type mismatch in launched event at " + path + " with value found: " + valueFound.toString) match
            case None => false.pure[SkijaUpdateT[IO, Float, SkijaClip, String, Event]]
            case Some(Right(event)) =>
              SkijaUpdate.raiseEvents[IO, Float, SkijaClip, String, Event](List(event)).as(true)
            case Some(Left(error)) =>
              SkijaUpdate.raiseError[IO, Float, SkijaClip, String, Event, Boolean](error)
      )
    end launchedEvent

    def resource[Event](supervisor : Supervisor[IO]) : ResourceWidget[Widget[Event], IO] =
      resourceWidget[
        Widget,
        SkijaUpdate[IO, Float, SkijaClip, String, *, *],
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

    gapPadding(ffi)(
      Paddings(10f, 10f, 10f, 10f)
    )(
      imageUrl[IO[Image], Widget[ApplicationRequest], Image](
        name = "image",
        resourceInit = resourceInit(_, preInit.globalSupervisor, _),
        imageSource = downloadImage("https://i.pinimg.com/1200x/1b/6e/8c/1b6e8c66f6d302c0c0156104a52a32be.jpg"),
        imageWidget = image(_, ffi),
        placeholder = text("Wait.", SkijaTextStyle(new Font(Typeface.makeDefault(), 28), new Paint().setColor(0xFF8484A4)))
      ).clip(
        SkijaClip.Shapes.round
      )
    )
  end main
end ImageExample