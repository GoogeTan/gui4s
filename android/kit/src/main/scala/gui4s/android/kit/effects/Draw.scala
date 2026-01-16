package gui4s.android.kit.effects

import android.graphics.drawable.Drawable
import android.graphics.{Bitmap, Canvas as AndroidCanvas}
import gui4s.android.skia
import gui4s.android.skia.canvas.*
import org.jetbrains.skia.*
import org.jetbrains.skia.paragraph.*

import java.nio.ByteBuffer

type Draw[IO[_]] = ReaderT[IO, Canvas, Unit]

object Draw:
  given[IO[_] : Applicative]: Canvased[ReaderT[IO, Canvas, *]] = Canvased.given_Canvased_ReaderT

  given monoidInstance[IO[_] : Monad] : Monoid[Draw[IO]] =
    catnip.syntax.all.applicativesAreMonoids[ReaderT[IO, Canvas, *]]
  end monoidInstance

  def drawAt[IO[_] : Sync](whatToDraw : Draw[IO], x : Float, y : Float) : Draw[IO] =
    skia.canvas.drawAt(x, y, whatToDraw)
  end drawAt

  def drawImage[IO[_] : Sync](image : Image) : Draw[IO] =
    skia.canvas.drawImage(image)
  end drawImage

  def drawText[IO[_] : Sync](text : skia.SkijaPlacedText) : Draw[IO] =
    skia.canvas.drawText(text)
  end drawText

  def drawClipped[IO[_] : Sync](path: Clip, original: Draw[IO]): Draw[IO] =
    withClipedPath(path, original)
  end drawClipped

  def drawParagraph[IO[_] : Sync](paragraph : Paragraph) : Draw[IO] =
    skia.canvas.drawParagraph(paragraph)
  end drawParagraph

  def drawAndroidDrawable[IO[_] : Sync](drawable : Drawable) : Draw[IO] =
    skia.canvas.drawAndroidDrawable(drawable)
  end drawAndroidDrawable
end Draw
