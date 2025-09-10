package gui4s.desktop.example.cats

import catnip.ForeignFunctionInterface
import catnip.effect.SyncForeignFunctionInterface
import cats.*
import cats.effect.std.Dispatcher
import cats.effect.{ExitCode, IO, IOApp, Resource}
import cats.syntax.all.*
import gui4s.core.geometry.*
import gui4s.core.layout.Sized
import gui4s.core.layout.rowcolumn.*
import gui4s.desktop.kit.common.*
import gui4s.desktop.kit.cats.*
import gui4s.desktop.kit.cats.effects.OuterPlace.given
import gui4s.desktop.kit.cats.effects.{ApplicationRequest, DownEvent, OuterPlace}
import gui4s.desktop.kit.cats.widgets.*
import gui4s.desktop.kit.generic.ContainerPlacementError
import gui4s.desktop.skija.*
import gui4s.glfw.{OglGlfwWindow, WindowCreationSettings}
import io.github.humbleui.skija.*
import io.github.humbleui.skija.shaper.Shaper

object GridExample extends IOApp:
  given ffi : ForeignFunctionInterface[IO] = SyncForeignFunctionInterface[IO]

  final case class PreInit(shaper : Shaper, globalTextCache : TextCache[IO])

  def preInit(backend : gui4s.desktop.kit.common.SkijaBackend[IO, Long, OglGlfwWindow, DownEvent]) : Resource[IO, PreInit] =
    for
      shaper <- backend.skija.createShaper
      cache : TextCache[IO] <- ScalacacheCache()
    yield PreInit(shaper, cache)
  end preInit

  override def run(args: List[String]): IO[ExitCode] =
    Dispatcher.sequential[IO].use(
      dispatcher =>
        desktopApp(
          preInit = preInit,
          main = main,
          updateLoopExecutionContext = this.runtime.compute,
          drawLoopExecutionContext = MainThread,
          settings = WindowCreationSettings(
            title = "Gui4s nested containers example",
            size = Rect(620f, 480f),
            visible = true,
            resizeable = true,
            debugContext = true
          ),
          unsafeRunF = dispatcher.unsafeRunAndForget
        )
    )
  end run

  def main(preInit : PreInit) : DesktopWidget[ApplicationRequest] =
    def gridExample[Event](numbers : List[Int]) : DesktopWidget[Event] =
      val spaceBetween : PlacementStrategy[OuterPlace, InfinityOr[Float], List, Float] = PlacementStrategy.ErrorIfInfinity(
        PlacementStrategy.SpaceBetween[OuterPlace, List, Float],
        ContainerPlacementError.English.withSpaceBetweenStrategy
      )
      val begin = OneElementPlacementStrategy.Begin[OuterPlace, InfinityOr[Float], Float]
      linearContainer[Event](
        mainAxis = Axis.Vertical,
        mainAxisStrategy = spaceBetween,
        additionalAxisStrategy = begin,
        children =
          numbers.map:
            lineIndex =>
              linearContainer[Event](
                mainAxis = Axis.Horizontal,
                mainAxisStrategy = spaceBetween,
                additionalAxisStrategy = begin,
                children =
                  numbers.map:
                    lineJindex =>
                      text(preInit.shaper, preInit.globalTextCache)(
                        lineIndex.toString + ":" + lineJindex.toString,
                        SkijaTextStyle(new Font(Typeface.makeDefault(), 28), new Paint().setColor(0xFF8484A4))
                      )
              )
      )
    end gridExample

    gridExample((0 until 10).toList)
  end main
end GridExample
