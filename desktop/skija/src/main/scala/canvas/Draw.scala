package gui4s.desktop.skija
package canvas

import canvas.Canvased.applyCanvasFFI

import catnip.ForeignFunctionInterface
import cats.FlatMap
import io.github.humbleui.skija.paragraph.Paragraph
import io.github.humbleui.skija.{Image, Paint, Path}

def clear[F[_]: {FlatMap, ForeignFunctionInterface, Canvased}](color : Int) : F[Unit] =
  applyCanvasFFI(_.clear(color))
end clear

def drawText[F[_]: {FlatMap, ForeignFunctionInterface, Canvased}](text: SkijaPlacedText) : F[Unit] =
  applyCanvasFFI(_.drawTextBlob(text.textBlob, 0, 0, text.paint))
end drawText

def drawImage[IO[_] : {FlatMap, ForeignFunctionInterface, Canvased}](image: Image): IO[Unit] =
  applyCanvasFFI(_.drawImage(image, 0f, 0f))
end drawImage

def drawPath[IO[_] : {FlatMap, ForeignFunctionInterface, Canvased}](path : Path, paint: Paint) : IO[Unit] =
  applyCanvasFFI(_.drawPath(path, paint))
end drawPath

def drawParagraph[F[_] : {FlatMap, ForeignFunctionInterface, Canvased}](paragraph: Paragraph): F[Unit] =
  gui4s.desktop.skija.canvas.Canvased.applyCanvasFFI(paragraph.paint(_, 0f, 0f))
end drawParagraph