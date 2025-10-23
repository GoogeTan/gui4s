package gui4s.desktop.example.cats

import cats.data.EitherT
import cats.effect.*
import glfw4s.core.*
import glfw4s.jvm.types.*
import gui4s.core.geometry.*
import gui4s.core.kit.ContainerPlacementError
import gui4s.core.layout.rowcolumn.*
import gui4s.desktop.kit.common.*
import gui4s.desktop.kit.common.effects.*
import gui4s.desktop.kit.common.widgets.*
import gui4s.desktop.skija.*
import io.github.humbleui.skija.*
import io.github.humbleui.skija.shaper.*

object GridExample extends CatsApp:
  final case class PreInit(shaper : Shaper, globalTextCache : TextCache[CatsIO])

  def preInit(backend : SkijaBackend[CatsIO, Resource[CatsIO, *], IO, GLFWmonitor, GLFWwindow, DownEvent]) : Resource[CatsIO, PreInit] =
    for
      shaper <- createShaper[CatsIO]
      cache : TextCache[CatsIO] <- ScalacacheCache()
    yield PreInit(shaper, cache)
  end preInit

  val settings = WindowCreationSettings(
    title = "Gui4s nested containers example example",
    width = 620,
    height = 480,
  )

  def main(preInit : PreInit) : DesktopWidget[CatsIO, ApplicationRequest] =
    def gridExample[Event](numbers : List[Int]) : DesktopWidget[CatsIO, Event] =
      val spaceBetween: PlacementStrategy[OuterPlaceT[CatsIO], InfinityOr[Float], List, Float] =
        PlacementStrategy.ErrorIfInfinity[OuterPlaceT[CatsIO], Float, List, Throwable](
          PlacementStrategy.SpaceBetween[OuterPlaceT[CatsIO], List, Float],
          ContainerPlacementError.English.withSpaceBetweenStrategy
        )
      val begin = OneElementPlacementStrategy.Begin[OuterPlaceT[CatsIO], InfinityOr[Float], Float]

      linearContainer[CatsIO, Event](
        mainAxis = Axis.Vertical,
        mainAxisStrategy = spaceBetween,
        additionalAxisStrategy = begin,
        children =
          numbers.map:
            lineIndex =>
              linearContainer[CatsIO, Event](
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
