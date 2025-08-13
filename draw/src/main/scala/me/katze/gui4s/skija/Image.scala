package me.katze.gui4s.skija

import catnip.ForeighFunctionInterface
import cats.Monad
import io.github.humbleui.skija.{Canvas, Image}

def drawImage_[IO[_]](ffi : ForeighFunctionInterface[IO], canvas : Canvas, image: Image) : IO[Unit] =
  ffi(canvas.drawImage(image, 0, 0))
end drawImage_

def drawImage[IO[_] : Monad](ffi : ForeighFunctionInterface[IO], image : Image) : SkijaDraw[IO] =
  SkijaDrawLoud.getCanvas[IO].flatMap(drawImage_(ffi.mapK(SkijaDrawLoud.liftK), _ : Canvas, image))
end drawImage
