package me.katze.gui4s.example
package examples

import api.*
import api.effects.SkijaDownEvent.{catchExternalEvent, eventOfferingCallbacks, extractMouseClickEvent}
import api.effects.SkijaPlace.TextCache
import api.effects.{*, given}
import api.widget.*
import app.{SkijaPlacedWidget, SkijaWidget, skijaGlfwApp, skijaGlfwCatsApp}
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
import me.katze.gui4s.geometry.*
import me.katze.gui4s.glfw.{OglGlfwWindow, WindowCreationSettings}
import me.katze.gui4s.layout.Sized
import me.katze.gui4s.layout.rowcolumn.{ManyElementsPlacementStrategy, OneElementPlacementStrategy}
import me.katze.gui4s.skija.*
import me.katze.gui4s.widget.library.*
import me.katze.gui4s.widget.library.decorator.*
import me.katze.gui4s.widget.{Path, library}
import scalacache.caffeine.CaffeineCache

import scala.reflect.Typeable

object GridExample extends IOApp:
  given ffi : ForeighFunctionInterface[IO] = SyncForeighFunctionInterface[IO]

  private type Widget[Event] = SkijaWidget[IO, Float, SkijaClip, String, String, Event, SkijaDownEvent[Float]]

  type PreInit = (shaper : Shaper, globalTextCache : TextCache[IO])

  def preInit(backend : SkijaBackend[IO, Long, OglGlfwWindow, SkijaDownEvent[Float]]) : Resource[IO, PreInit] =
    for
      shaper <- backend.skija.createShaper
      cache : TextCache[IO] <- Resource.eval(CaffeineCache[IO, (String, SkijaTextStyle, Option[Float]), Sized[Float, SkijaPlacedText]])
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
        title = "Gui4s nested layouts example",
        size = Rect(620f, 480f),
        visible = true,
        resizeable = true,
        debugContext = true
      )
    )
  end run

  def main(preInit : PreInit)(using backend : SkijaBackend[IO, Long, OglGlfwWindow, SkijaDownEvent[Float]]) : Widget[ApplicationRequest] =
    def text[Event] : TextWidget[Widget[Event]] =
        skijaText(ffi, preInit.shaper, preInit.globalTextCache)
    end text

    def linearContainer[Event] : LinearContainer[Widget[Event], SkijaOuterPlaceT[IO, Float, String], List, Float, Axis] =
      skijaLinearContainer(
        skijaContainer(
          ffi,
          [A : Order, B] => v => f => orderedListProcessing(v)(f),
        ),
        [A, B] => (a, b) => a.zip(b)
      )
    end linearContainer

    def gridExample[Event](numbers : List[Int]) : Widget[Event] =
      linearContainer[Event](
        mainAxis = Axis.Vertical,
        mainAxisStrategy = ManyElementsPlacementStrategy.SpaceBetween(ENErrors.withSpaceBetweenStrategy),
        additionalAxisStrategy = OneElementPlacementStrategy.Begin,
        children =
          numbers.map:
            lineIndex =>
              linearContainer[Event](
                mainAxis = Axis.Horizontal,
                mainAxisStrategy = ManyElementsPlacementStrategy.SpaceBetween(ENErrors.withSpaceBetweenStrategy),
                additionalAxisStrategy = OneElementPlacementStrategy.Begin,
                children =
                  numbers.map:
                    lineJindex =>
                      text(
                        lineIndex.toString + ":" + lineJindex.toString,
                        SkijaTextStyle(new Font(Typeface.makeDefault(), 28), new Paint().setColor(0xFF8484A4))
                      )
              )
      )
    end gridExample

    gridExample((0 until 10).toList)
  end main
end GridExample