package gui4s.android.skia.canvas

import org.jetbrains.skia.paragraph.*
import org.jetbrains.skia.*
import cats.*
import cats.data.*
import cats.effect.*
import Canvased.*

def clear[F[_]: {Sync, Canvased}](color : Int) : F[Unit] =
  applyCanvasFFI(_.clear(color))
end clear

def drawText[F[_]: {Sync, Canvased}](text: gui4s.android.skia.SkijaPlacedText) : F[Unit] =
  applyCanvasFFI(_.drawTextBlob(text.textBlob, 0, 0, text.paint))
end drawText

def drawImage[IO[_] : {Sync, Canvased}](image: Image): IO[Unit] =
  applyCanvasFFI(_.drawImage(image, 0f, 0f))
end drawImage

def drawPath[IO[_] : {Sync, Canvased}](path : Path, paint: Paint) : IO[Unit] =
  applyCanvasFFI(_.drawPath(path, paint))
end drawPath

def drawParagraph[F[_] : {Sync, Canvased}](paragraph: Paragraph): F[Unit] =
  applyCanvasFFI(paragraph.paint(_, 0f, 0f))
end drawParagraph