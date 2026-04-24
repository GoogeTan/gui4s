package gui4s.desktop.example.shared

import java.util.concurrent.TimeUnit

import scala.concurrent.duration.*

import gui4s.core.widget.library.animation.Animation
import gui4s.core.widget.library.animation.Easing
import gui4s.core.widget.library.animation.NormedVectorSpace.floatNormedVectorSpace
import gui4s.core.widget.library.animation.TweenAnimation

import gui4s.desktop.kit.effects.*
import gui4s.desktop.kit.widgets.*
import gui4s.desktop.skija.Font
import gui4s.desktop.skija.Image
import gui4s.desktop.skija.Paint
import gui4s.desktop.skija.SkijaTextStyle
import gui4s.desktop.skija.Typeface

def animationExample[Event](
  initialization: InitializationWidget,
  text: TextWidget,
  typeface: Typeface,
)(using ScrollWidget): DesktopWidget[Event] =

  val floatAnimation: Animation[Float, Duration] = TweenAnimation(
    easing = Easing.FastOutSlowIn,
    duration = Duration(50, TimeUnit.MILLISECONDS)
  )
  initialization[Image, Event](
    name = "image",
    effectToRun = downloadImage("https://4pda.to/s/qirtdz1qChDeJB8Bcsz2XUtscYQoC8Vfk3E2i62x51wPrcI3rKcz0Gz1z0BWwKe.png"),
    body = loadedImageData =>
      columnWidget(
        children = List.fill(7)(imageWidget[Event](loadedImageData)),
        verticalPlacementStrategy = LinearContainerPlacementStrategy.Begin(0f),
        horizontalPlacementStrategy = LinearContainerPlacementStrategy.Center(0f, gui4s.core.kit.ContainerPlacementError.English),
      ).verticallyScrollable(
        stateName = "scroll_state",
        scrollAnimation = floatAnimation
      ),
    placeholder = text("Please, wait.", SkijaTextStyle(new Font(typeface, 28), new Paint().setColor(0xFF8484A4))),
  )
end animationExample
