package gui4s.desktop.kit
package common.effects

import catnip.ForeignFunctionInterface
import catnip.syntax.all.given
import cats.*
import cats.data.ReaderT
import gui4s.desktop.skija
import gui4s.desktop.skija.canvas.*
import io.github.humbleui.skija.*
import io.github.humbleui.skija.paragraph.*

type Draw[IO[_]] = ReaderT[IO, Canvas, Unit]

object Draw:
  given[IO[_] : Applicative]: Canvased[Draw[IO]] = ReaderT.ask

  given monoidInstance[IO[_] : Monad] : Monoid[Draw[IO]] =
    summon
  end monoidInstance

  def drawAt[IO[_] : {Monad, ForeignFunctionInterface as ffi}](whatToDraw : Draw[IO], x : Float, y : Float) : Draw[IO] =
    drawAt(whatToDraw, x, y)
  end drawAt

  def drawImage[IO[_] : {Monad, ForeignFunctionInterface as ffi}](image : Image) : Draw[IO] =
    drawImage(image)
  end drawImage

  def drawText[IO[_] : ForeignFunctionInterface as ffi](text : skija.SkijaPlacedText) : Draw[IO] =
    drawText(text)
  end drawText

  def drawClipped[IO[_] : {Monad, ForeignFunctionInterface as ffi}](path: Clip, original: Draw[IO]): Draw[IO] =
    clipToPath(path, original)
  end drawClipped

  def drawParagraph[IO[_] : {Monad, ForeignFunctionInterface as ffi}](paragraph : Paragraph) : Draw[IO] =
    drawParagraph(paragraph)
  end drawParagraph
end Draw
