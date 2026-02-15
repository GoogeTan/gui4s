package gui4s.desktop.example

import catnip.syntax.all.given
import cats.effect.*
import gui4s.core.geometry.*
import gui4s.core.layout.rowcolumn
import gui4s.desktop.kit.effects.*
import gui4s.desktop.kit.widgets.*
import gui4s.desktop.kit.widgets.decorator.*
import gui4s.desktop.skija.{Font, Image, Paint, SkijaTextStyle, Typeface}
import io.github.humbleui.skija.BlendMode

def inverseColorPaint : Paint =
  val inverseColor = new Paint
  inverseColor.setColor(0xFFAAAAAA)
  inverseColor.setBlendMode(BlendMode.DIFFERENCE)
  inverseColor.setAntiAlias(true)
  inverseColor
end inverseColorPaint

def headerTextStyle(typeface : Typeface): SkijaTextStyle =
  SkijaTextStyle(new Font(typeface, 72 * 2), inverseColorPaint)
end headerTextStyle

def pleaseWaitTextStyle(typeface : Typeface): SkijaTextStyle =
  SkijaTextStyle(new Font(typeface, 72 * 2), inverseColorPaint)
end pleaseWaitTextStyle

def imageExample(
  text : TextWidget,
  typeface: Typeface,
  initialization: InitializationWidget,
) : DesktopWidget[Nothing] =
  given Ordering[Rect[Float]] = Ordering.by(point => math.max(point.width, point.width))

  val centerPlacement = rowcolumn.OneElementPlacementStrategy.Center[PlacementEffectC[IO], Float]
  val beginPlacement = rowcolumn.OneElementPlacementStrategy.Begin[PlacementEffectC[IO], Float, Float]
  val textPlacement = rowcolumn.PlacementStrategy.Zip(
    beginPlacement,
    beginPlacement
  )

  initialization(
    name = "mononokeImage",
    effectToRun = downloadImage("https://i.pinimg.com/736x/c6/f2/41/c6f241cff25453bca4c861009e32d141.jpg"),
    body = image =>
      imageWidget[Nothing](image)
        .withForeground(
          foreground = text[Nothing]("Princess Mononoke", headerTextStyle(typeface)),
          placement = textPlacement
        ),
    placeholder = text(
      "Please wait...",
      pleaseWaitTextStyle(typeface)
    )
  )
end imageExample
