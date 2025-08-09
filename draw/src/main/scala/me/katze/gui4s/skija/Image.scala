package me.katze.gui4s.skija

import catnip.ForeighFunctionInterface
import io.github.humbleui.skija.{Canvas, Image}

def drawImage[IO[_]](ffi : ForeighFunctionInterface[IO], canvas : Canvas, image: Image) : IO[Unit] =
  ffi(canvas.drawImage(image, 0, 0))
end drawImage