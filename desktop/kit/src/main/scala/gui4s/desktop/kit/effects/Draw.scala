package gui4s.desktop.kit
package effects

import cats._
import cats.data.ReaderT
import cats.effect._
import io.github.humbleui.skija._
import io.github.humbleui.skija.paragraph._
import io.github.humbleui.types.Rect.makeWH

import gui4s.core.geometry.Rect

import gui4s.desktop.skija
import gui4s.desktop.skija.SkPaint
import gui4s.desktop.skija.canvas._


type DrawM[T] = ReaderT[SyncIO, Canvas, T]
type Draw = DrawM[Unit]

object Draw:
  given Canvased[ReaderT[SyncIO, Canvas, *]] = Canvased.given_Canvased_ReaderT

  given monoidInstance : Monoid[Draw] =
    catnip.syntax.all.applicativesAreMonoids[ReaderT[SyncIO, Canvas, *]]
  end monoidInstance

  def drawAt(whatToDraw : Draw, x : Float, y : Float) : Draw =
    skija.canvas.drawAt(x, y, whatToDraw)
  end drawAt

  def drawImage(image : Image) : Draw =
    skija.canvas.drawImage(image)
  end drawImage

  def drawText(text : skija.SkijaPlacedText) : Draw =
    skija.canvas.drawText(text)
  end drawText

  def drawClipped(path: Clip, original: Draw): Draw =
    withClipedPath(path, original)
  end drawClipped

  def drawParagraph(paragraph : Paragraph) : Draw =
    skija.canvas.drawParagraph(paragraph)
  end drawParagraph

  def drawCursor(
                  paragraph: Paragraph,
                  cursorPos : Int,
                  cursorPaint: Paint
                ) : Draw =
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

  def drawBrush(brush : skija.Brush, size : Rect[Float]) : Draw =
    Canvased.applyCanvasFFI:
      canvas =>
        canvas.drawRect(makeWH(size.width, size.height), brush(size, SkPaint()).toSkia)
  end drawBrush

  def drawBrush(brush : skija.Brush, size : Rect[Float], path : Path) : Draw =
    Canvased.applyCanvasFFI:
      canvas =>
        canvas.drawPath(path, brush(size, SkPaint()).toSkia)
  end drawBrush

  def drawBorder(path : Path, size : Rect[Float], brush : skija.Brush, paint : SkPaint) : Draw =
    Canvased.applyCanvasFFI:
      canvas =>
        canvas.drawPath(path, brush(size, paint).toSkia)
  end drawBorder
end Draw
