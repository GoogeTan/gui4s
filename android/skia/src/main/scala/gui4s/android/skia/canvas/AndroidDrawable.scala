package gui4s.android.skia.canvas

import android.graphics.{Bitmap, Canvas as AndroidCanvas}
import android.graphics.drawable.Drawable
import cats.effect.Sync
import gui4s.android.skia
import org.jetbrains.skia.Canvas
import java.nio.ByteBuffer
import org.jetbrains.skia.{ColorType, ColorAlphaType, ImageInfo, Image}

def drawAndroidDrawable[IO[_] : {Sync, Canvased}](drawable: android.graphics.drawable.Drawable): IO[Unit] =
  skia.canvas.Canvased.applyCanvasFFI(
    drawAndroidDrawableDirty(_, drawable)
  )
end drawAndroidDrawable

def drawAndroidDrawableDirty(skiaCanvas: Canvas, drawable: Drawable) = {
  skiaCanvas.drawImage(drawable.toSkiaImage(), 0, 0)
}

extension (drawable: Drawable)
  def toSkiaImage() : Image =
    val width = if (drawable.getIntrinsicWidth > 0) drawable.getIntrinsicWidth else 1
    val height = if (drawable.getIntrinsicHeight > 0) drawable.getIntrinsicHeight else 1
    val androidBitmap = Bitmap.createBitmap(width, height, Bitmap.Config.ARGB_8888)
    val androidCanvas = AndroidCanvas(androidBitmap)
    drawable.setBounds(0, 0, androidCanvas.getWidth, androidCanvas.getHeight)
    drawable.draw(androidCanvas)
    val skiaImage = androidBitmap.toSkiaImage()
    androidBitmap.recycle()
    skiaImage

extension (bitmap: Bitmap)
  def toSkiaImage(): Image =
    val width = bitmap.getWidth
    val height = bitmap.getHeight
    val bytes = new Array[Byte](bitmap.getByteCount)
    val buffer = ByteBuffer.wrap(bytes)
    bitmap.copyPixelsToBuffer(buffer)
    val imageInfo = new ImageInfo(
      width,
      height,
      ColorType.RGBA_8888,
      ColorAlphaType.PREMUL,
      null 
    )

    val rowBytes = bitmap.getRowBytes

    Image.Companion.makeRaster(imageInfo, bytes, rowBytes)
  end toSkiaImage
end extension