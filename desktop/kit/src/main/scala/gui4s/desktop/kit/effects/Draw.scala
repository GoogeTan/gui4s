package gui4s.desktop.kit
package effects

import cats._
import cats.data.ReaderT
import cats.effect.Sync
import io.github.humbleui.skija._
import io.github.humbleui.skija.paragraph._

import gui4s.desktop.skija
import gui4s.desktop.skija.canvas._

type Draw[IO[_]] = ReaderT[IO, Canvas, Unit]

object Draw:
  given[IO[_] : Applicative]: Canvased[ReaderT[IO, Canvas, *]] = Canvased.given_Canvased_ReaderT

  given monoidInstance[IO[_] : Monad] : Monoid[Draw[IO]] =
    catnip.syntax.all.applicativesAreMonoids[ReaderT[IO, Canvas, *]]
  end monoidInstance

  def drawAt[IO[_] : Sync](whatToDraw : Draw[IO], x : Float, y : Float) : Draw[IO] =
    skija.canvas.drawAt(x, y, whatToDraw)
  end drawAt

  def drawImage[IO[_] : Sync](image : Image) : Draw[IO] =
    skija.canvas.drawImage(image)
  end drawImage

  def drawText[IO[_] : Sync](text : skija.SkijaPlacedText) : Draw[IO] =
    skija.canvas.drawText(text)
  end drawText

  def drawClipped[IO[_] : Sync](path: Clip, original: Draw[IO]): Draw[IO] =
    withClipedPath(path, original)
  end drawClipped

  def drawParagraph[IO[_] : Sync](paragraph : Paragraph) : Draw[IO] =
    skija.canvas.drawParagraph(paragraph)
  end drawParagraph

  def drawCursor[IO[_] : Sync](
                                paragraph: Paragraph,
                                cursorPos : Int,
                                cursorPaint: Paint
                              ) : Draw[IO] =
    Canvased.applyCanvasFFI(canvas =>
      // Честно говоря, я не знаю почему это так, но без этого не работает.
      // Если брать только первое, то на ненулевой позиции будет пустой массив, если только второе,
      // то будет пустой массив на нулевой позиции
      val (x0, y0, x1, y1) =
        val rects = paragraph.getRectsForRange(
          cursorPos,
          cursorPos + 1,
          RectHeightMode.MAX,
          RectWidthMode.TIGHT
        )
        if rects.nonEmpty then
          val cursorRect = rects.last.getRect
          val cursorX = cursorRect.getLeft + cursorPaint.getStrokeWidth / 2
          val topY = cursorRect.getTop
          val bottomY = cursorRect.getBottom
          (cursorX, topY, cursorX, bottomY)
        else
          val rects = paragraph.getRectsForRange(
            cursorPos - 1,
            cursorPos,
            RectHeightMode.MAX,
            RectWidthMode.TIGHT
          )
          val cursorRect = rects.last.getRect
          val cursorX = cursorRect.getRight + cursorPaint.getStrokeWidth / 2
          val topY = cursorRect.getTop
          val bottomY = cursorRect.getBottom
          (cursorX, topY, cursorX, bottomY)
        end if
      canvas.drawLine(x0, y0, x1, y1, cursorPaint)
    )
  end drawCursor
end Draw
