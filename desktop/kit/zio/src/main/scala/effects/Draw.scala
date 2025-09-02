package gui4s.desktop.kit.zio
package effects

import catnip.ForeignFunctionInterface
import catnip.syntax.all.given
import cats.*
import gui4s.desktop.skija
import gui4s.desktop.skija.SkijaDraw
import io.github.humbleui.skija.*
import gui4s.desktop.kit.effects.Draw as GenericDraw
import io.github.humbleui.skija.shaper.Shaper
import zio.*
import zio.interop.catz.*

type Draw = GenericDraw[Task]

object Draw:
  given monoidInstance : Monoid[Draw] =
    GenericDraw.monoidInstance[Task]
  end monoidInstance

  def drawAt(whatToDraw : Draw, x : Float, y : Float) : Draw =
    GenericDraw.drawAt(whatToDraw, x, y)
  end drawAt

  def drawImage(image : Image) : Draw =
    GenericDraw.drawImage(image)
  end drawImage

  def drawText(text : skija.SkijaPlacedText) : Draw =
    GenericDraw.drawText(text)
  end drawText

  def drawClipped(path: Clip, original: Draw): Draw =
    GenericDraw.drawClipped(path, original)
  end drawClipped
  
  def makeShaper : RIO[Scope, Shaper] =
    ZIO.acquireRelease(
      ffi.delay(
        Shaper.make()
      )
    )(
      shaper =>
        ffi.delay(
          shaper.close()
        ).orElseSucceed(())
    )
  end makeShaper  
end Draw
