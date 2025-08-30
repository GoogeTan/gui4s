package gui4s.desktop.kit
package effects

import catnip.ForeignFunctionInterface
import cats.effect.IO
import gui4s.desktop.skija
import gui4s.desktop.skija.SkijaDraw
import io.github.humbleui.skija.*

type Draw[IO[_]] = SkijaDraw[IO]

object Draw:
  def drawAt[IO[_]](ffi : ForeignFunctionInterface[IO])(whatToDraw : Draw[IO], x : Float, y : Float) : Draw[IO] =
    skija.drawAt(ffi, whatToDraw, x, y)
  end drawAt

  def drawImage[IO[_]](ffi : ForeignFunctionInterface[IO])(image : Image) : Draw[IO] =
    skija.drawImage(ffi, image)
  end drawImage

  def drawText[IO[_]](ffi : ForeignFunctionInterface[IO])(text : skija.SkijaPlacedText) : Draw[IO] =
    skija.drawText(ffi, text)
  end drawText

  def drawClipped[IO[_]](ffi : ForeignFunctionInterface[IO])(path: Clip, original: Draw): Draw[IO] =
    skija.clipToPath(ffi, path, original)
  end drawClipped
end Draw
