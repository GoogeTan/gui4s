package me.katze.gui4s.example
package examples

import api.*
import api.effects.SkijaDownEvent.{eventOfferingCallbacks, extractMouseClickEvent}
import api.effects.{*, given}
import api.widget.*
import app.{SkijaPlacedWidget, SkijaWidget, skijaGlfwCatsApp}
import place.*
import skija.SkijaBackend

import catnip.ForeighFunctionInterface
import catnip.cats.effect.SyncForeighFunctionInterface
import catnip.syntax.all.{*, given}
import cats.*
import cats.effect.{ExitCode, IO, IOApp, Resource}
import cats.syntax.all.*
import io.github.humbleui.skija.*
import io.github.humbleui.skija.shaper.Shaper
import me.katze.gui4s
import me.katze.gui4s.example
import me.katze.gui4s.geometry.*
import me.katze.gui4s.glfw.{OglGlfwWindow, WindowCreationSettings}
import me.katze.gui4s.layout.Sized
import me.katze.gui4s.layout.rowcolumn.{ManyElementsPlacementStrategy, OneElementPlacementStrategy}
import me.katze.gui4s.skija.*
import me.katze.gui4s.widget.Path
import me.katze.gui4s.widget.library.*
import me.katze.gui4s.widget.library.decorator.*
import scalacache.caffeine.CaffeineCache

object ClickabeExample extends IOApp:
  given ffi : ForeighFunctionInterface[IO] = SyncForeighFunctionInterface[IO]

  private type PlacedWidget[Event] = SkijaPlacedWidget[IO, Float, SkijaClip, String, String, Event, SkijaDownEvent[Float]]
  private type Widget[Event] = SkijaWidget[IO, Float, SkijaClip, String, String, Event, SkijaDownEvent[Float]]

  type PreInit = (shaper : Shaper, globalTextCache : TextCache[IO])

  def preInit(backend : SkijaBackend[IO, Long, OglGlfwWindow, SkijaDownEvent[Float]]) : Resource[IO, PreInit] =
    for
      shaper <- backend.skija.createShaper
      cache : TextCache[IO] <- Resource.eval(CaffeineCache[IO, (String, SkijaTextStyle, Option[Float]), Sized[Float, SkijaPlacedText]]).map(scalacacheCache)
    yield (shaper, cache)
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
        title = "Gui4s clickable example",
        size = Rect(620f, 480f),
        visible = true,
        resizeable = true,
        debugContext = true
      )
    )
  end run

  def main(preInit : PreInit)(using backend : SkijaBackend[IO, Long, OglGlfwWindow, SkijaDownEvent[Float]]) : Widget[SkijaApplicationRequest] =
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
      def onClick(event : Event) : Widget[Event] =
        skijaClickCatcher[
          IO,
          Float,
          SkijaClip,
          String,
          SkijaPlaceT[IO, Float, String],
          SkijaDraw[IO],
          SkijaRecomposition[IO],
          SkijaDownEvent[Float],
          Event
        ](
          eventCatcher,
          extractMouseClickEvent,
          backend.mousePosition
        )(event)(value)
      end onClick
    end extension

    def statefulWidget: StatefulWidget[Widget, SkijaUpdate[IO, Float, SkijaClip, String, *, *], [Value] =>> Value => SkijaRecomposition[IO]] = skijaStateful(
      (value: Any, path: Path) => "Error in stateful typechecking at " + path.toString + " with value [" + value.toString + "]"
    )

    def text[Event] : TextWidget[Widget[Event]] =
        skijaText(ffi, preInit.shaper, preInit.globalTextCache)
    end text

    def linearLayout[Event] : LinearContainer[Widget[Event], SkijaOuterPlaceT[IO, Float, String], List, Float, Axis] =
      skijaLinearContainer(
        skijaContainer(ffi,
          [A : Order, B] => v => f => orderedListProcessing(v)(f)
        ),
        [A, B] => (a, b) => a.zip(b)
      )
    end linearLayout

    def clickExample[Event](numbers : List[Int]): Widget[Event] =
      linearLayout(
        mainAxis = Axis.Vertical,
        mainAxisStrategy = ManyElementsPlacementStrategy.Begin(0f),
        additionalAxisStrategy = OneElementPlacementStrategy.Center(ContainerPlacementError.English.withCenterStrategy),
        children = numbers.map:
          lineNumber =>
            statefulWidget[Int, Event, Unit](
              name = "line-" + lineNumber.toString,
              initialState = 0,
              eventHandler = (state, _, _) => (state + 1).pure[SkijaUpdateT[IO, Float, SkijaClip, String, Event]],
              body = state =>
                text(
                  "# " + lineNumber.toString + " : " + state.toString,
                  SkijaTextStyle(new Font(Typeface.makeDefault(), 24), new Paint().setColor(0xFF8484A4))
                ).onClick(())
            ),
      )
    end clickExample

    clickExample((0 until 10).toList)
  end main
end ClickabeExample