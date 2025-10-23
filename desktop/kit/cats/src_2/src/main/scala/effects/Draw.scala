package gui4s.desktop.kit.cats
package effects

import cats.*
import cats.effect.IO
import gui4s.desktop.kit.common.effects.Draw as GenericDraw
import gui4s.desktop.skija
import io.github.humbleui.skija.*

type Draw = GenericDraw[IO]

object Draw:
  given monoidInstance : Monoid[Draw] =
    GenericDraw.monoidInstance[IO]
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
end Draw
