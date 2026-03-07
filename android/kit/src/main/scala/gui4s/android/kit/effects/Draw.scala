package gui4s.android.kit.effects

import cats.effect.IO
import android.graphics.drawable.Drawable
import gui4s.android.skia
import gui4s.android.skia.canvas.*
import org.jetbrains.skia.*
import org.jetbrains.skia.paragraph.*

type Draw = ReaderT[IO, Canvas, Unit]

object Draw:
  given Canvased[ReaderT[IO, Canvas, *]] = Canvased.given_Canvased_ReaderT

  given monoidInstance : Monoid[Draw] =
    catnip.syntax.all.applicativesAreMonoids[ReaderT[IO, Canvas, *]]
  end monoidInstance

  def drawAt(whatToDraw : Draw, x : Float, y : Float) : Draw =
    skia.canvas.drawAt(x, y, whatToDraw)
  end drawAt

  def drawImage(image : Image) : Draw =
    skia.canvas.drawImage(image)
  end drawImage

  def drawText(text : skia.SkijaPlacedText) : Draw =
    skia.canvas.drawText(text)
  end drawText

  def drawClipped(path: Clip, original: Draw): Draw =
    withClipedPath(path, original)
  end drawClipped

  def drawParagraph(paragraph : Paragraph) : Draw =
    skia.canvas.drawParagraph(paragraph)
  end drawParagraph

  def drawAndroidDrawable(drawable : Drawable) : Draw =
    skia.canvas.drawAndroidDrawable(drawable)
  end drawAndroidDrawable
end Draw
