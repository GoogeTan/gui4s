package gui4s.desktop.kit
package widgets

import catnip.syntax.all.{*, given}
import catnip.syntax.transformer.given
import cats.*
import cats.effect.*
import cats.syntax.all.*
import gui4s.core.geometry.Rect
import gui4s.core.layout.Sized
import gui4s.desktop.kit.asset.*
import gui4s.desktop.kit.effects.*
import gui4s.desktop.skija.Image
import io.github.humbleui.skija.svg.{SVGDOM, SVGLength, SVGLengthContext, SVGLengthUnit}

def svgImageFromJavaResourcesWidget[Event](
  name : String,
  path : String,
  resourceWidget: ResourceWidget,
  onLoad : DesktopWidget[Event],
  onError : DesktopWidget[Event]
) : DesktopWidget[Event] =
  resourceWidget(
    name,
    svgFromJavaResources[IO, Resource[IO, *]](path)
  ):
    case Some(Some(value)) => imageWidget(value)
    case Some(None) => onError
    case None        => onLoad
end svgImageFromJavaResourcesWidget

def imageFromJavaResourcesWidget[Event](
  name : String,
  path : String,
  resourceWidget: ResourceWidget,
  onLoad : DesktopWidget[Event],
  onError : DesktopWidget[Event]
) : DesktopWidget[Event] =
  resourceWidget(
    name,
    imageFromJavaResources[IO, Resource[IO, *]](path)
  ):
    case Some(Some(value)) => imageWidget(value)
    case Some(None) => onError
    case None        => onLoad
end imageFromJavaResourcesWidget

def imageWidget[Event](image: Image): DesktopWidget[Event] =
  drawOnlyWidget(
    Sized(Draw.drawImage(image), Rect(image.getWidth.toFloat, image.getHeight.toFloat)).pure[PlacementEffect]
  )
end imageWidget

def imageWidget[Event](image: SVGDOM): DesktopWidget[Event] =
  drawOnlyWidget(
    sizeSVG(image).map(_.coflatMap(Draw.drawSVG))
  )
end imageWidget


def sizeSVG(dom : SVGDOM) : Place[SVGDOM] =
  PlacementEffect.liftFunction(bounds =>
    IO:
      val root = dom.getRoot
      val availableWidth = bounds.width.valueOr(Float.PositiveInfinity)
      val availableHeight = bounds.height.valueOr(Float.PositiveInfinity)
      val intrinsic = root.getIntrinsicSize(
        SVGLengthContext(
          availableWidth,
          availableHeight
        )
      )
      var svgWidth = intrinsic.getX
      var svgHeight = intrinsic.getY
  
      if (svgWidth <= 0 || svgHeight <= 0) {
        // Fallback for SVGs without explicit size (rare; intrinsicSize should handle viewBox)
        svgWidth = availableWidth//TODO
        svgHeight = availableHeight
      }
      val scale = Math.min(availableWidth / svgWidth, availableHeight / svgHeight)
      val targetWidth = svgWidth * scale
      val targetHeight = svgHeight * scale
      root.setWidth(new SVGLength(targetWidth, SVGLengthUnit.PX))
      root.setHeight(new SVGLength(targetHeight, SVGLengthUnit.PX))
      Sized(dom, Rect(targetWidth, targetHeight))
  )
end sizeSVG
