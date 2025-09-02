package gui4s.desktop.kit
package effects

import catnip.ForeignFunctionInterface
import catnip.syntax.all.given
import cats.*
import gui4s.desktop.skija
import gui4s.desktop.skija.SkijaDraw
import io.github.humbleui.skija.*

type Draw[IO[_]] = SkijaDraw[IO]

object Draw:
  given monoidInstance[IO[_] : Monad] : Monoid[Draw[IO]] =
    summon
  end monoidInstance

  def drawAt[IO[_] : {Monad, ForeignFunctionInterface as ffi}](whatToDraw : Draw[IO], x : Float, y : Float) : Draw[IO] =
    skija.drawAt(ffi, whatToDraw, x, y)
  end drawAt

  def drawImage[IO[_] : {Monad, ForeignFunctionInterface as ffi}](image : Image) : Draw[IO] =
    skija.drawImage(ffi, image)
  end drawImage

  def drawText[IO[_] : ForeignFunctionInterface as ffi](text : skija.SkijaPlacedText) : Draw[IO] =
    skija.drawText(ffi, text)
  end drawText

  def drawClipped[IO[_] : {Monad, ForeignFunctionInterface as ffi}](path: Clip, original: Draw[IO]): Draw[IO] =
    skija.clipToPath(ffi, path, original)
  end drawClipped
end Draw
