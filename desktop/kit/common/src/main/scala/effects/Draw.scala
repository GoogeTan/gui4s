package gui4s.desktop.kit
package common.effects

import catnip.syntax.all.given
import cats.*
import cats.data.ReaderT
import gui4s.desktop.skija
import gui4s.desktop.skija.canvas.*
import io.github.humbleui.skija.*
import io.github.humbleui.skija.paragraph.*
import cats.effect.Sync

type Draw[IO[_]] = ReaderT[IO, Canvas, Unit]

object Draw:
  given[IO[_] : Applicative]: Canvased[ReaderT[IO, Canvas, *]] = ReaderT.ask

  given monoidInstance[IO[_] : Monad] : Monoid[Draw[IO]] =
    summon
  end monoidInstance

  def drawAt[IO[_] : Sync](whatToDraw : Draw[IO], x : Float, y : Float) : Draw[IO] =
    drawAt(whatToDraw, x, y)
  end drawAt

  def drawImage[IO[_] : Sync](image : Image) : Draw[IO] =
    drawImage(image)
  end drawImage

  def drawText[IO[_] : Sync](text : skija.SkijaPlacedText) : Draw[IO] =
    drawText(text)
  end drawText

  def drawClipped[IO[_] : Sync](path: Clip, original: Draw[IO]): Draw[IO] =
    withClipedPath(path, original)
  end drawClipped

  def drawParagraph[IO[_] : Sync](paragraph : Paragraph) : Draw[IO] =
    drawParagraph(paragraph)
  end drawParagraph
end Draw
