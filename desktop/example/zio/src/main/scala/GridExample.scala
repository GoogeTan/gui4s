
package gui4s.desktop.example.zio

import catnip.ForeignFunctionInterface
import catnip.syntax.all.given
import gui4s.core.geometry.*
import gui4s.core.layout.Sized
import gui4s.core.layout.rowcolumn.{ManyElementsPlacementStrategy, OneElementPlacementStrategy}
import gui4s.desktop.kit.zio.*
import gui4s.desktop.kit.zio.effects.{*, given}
import gui4s.desktop.kit.zio.widgets.*
import gui4s.desktop.skija.*
import gui4s.glfw.{OglGlfwWindow, WindowCreationSettings}
import io.github.humbleui.skija.*
import io.github.humbleui.skija.shaper.Shaper
import scalacache.caffeine.CaffeineCache
import zio.*

object GridExample extends ZIOAppDefault:
  given ffi: ForeignFunctionInterface[Task] = new ZioForeignFunctionInterface(zio.Runtime.default)

  final case class PreInit(shaper: Shaper, globalTextCache: TextCache[Task])

  def preInit(backend: SkijaBackend[Task, Long, OglGlfwWindow, DownEvent]): ZIO[Scope, Throwable, PreInit] =
    for
      shaper <- backend.skija.createShaper
      cache: TextCache[Task] <- ZIO.fromEither(CaffeineCache[Task, (String, SkijaTextStyle, Option[Float]), Sized[Float, SkijaPlacedText]]).map(ScalacacheCache(_)).orDie
    yield PreInit(shaper, cache)
  end preInit

  override def run: ZIO[Any & ZIOAppArgs & Scope, Any, Any] =
    desktopApp[
      PreInit
    ](
      preInit = preInit,
      main = main,
      settings = WindowCreationSettings(
        title = "Gui4s nested containers example",
        size = Rect(620f, 480f),
        visible = true,
        resizeable = true,
        debugContext = true
      ),
      ffi = ffi,
    )
  end run

  def main(preInit: PreInit): DesktopWidget[ApplicationRequest] =
    def gridExample[Event](numbers: List[Int]): DesktopWidget[Event] =
      val spaceBetween = ManyElementsPlacementStrategy.ErrorIfInfinity(
        ManyElementsPlacementStrategy.SpaceBetween[OuterPlace, List, Float],
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
