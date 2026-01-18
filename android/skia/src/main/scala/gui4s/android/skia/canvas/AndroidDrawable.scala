package gui4s.android.skia.canvas

import android.graphics.{Bitmap, Canvas as AndroidCanvas}
import android.graphics.drawable.Drawable
import cats.effect.Sync
import gui4s.android.skia
import org.jetbrains.skia.Canvas

import java.nio.ByteBuffer
import org.jetbrains.skia.{ColorAlphaType, ColorType, Image, ImageInfo, Rect}

def drawAndroidDrawable[IO[_] : {Sync, Canvased}](drawable: android.graphics.drawable.Drawable): IO[Unit] =
  skia.canvas.Canvased.applyCanvasFFI(
    drawAndroidDrawableDirty(_, drawable)
  )
end drawAndroidDrawable

def drawAndroidDrawableDirty(skiaCanvas: Canvas, drawable: Drawable) = {
  skiaCanvas.drawImageRect(
    drawable.toSkiaImage(),
    Rect.makeXYWH(0, 0, drawable.getBounds.width(), drawable.getBounds.height())
  )
}

extension (drawable: Drawable)
  def toSkiaImage() : Image =
    val width = if (drawable.getBounds.width() > 0) drawable.getBounds.width()
    else if (drawable.getIntrinsicWidth > 0) drawable.getIntrinsicWidth
    else 100
    val height = if (drawable.getBounds.height() > 0) drawable.getBounds.height()
    else if (drawable.getIntrinsicHeight > 0) drawable.getIntrinsicHeight
    else 100
    drawable.setBounds(0, 0, width, height)
    val bitmap = Bitmap.createBitmap(width, height, Bitmap.Config.ARGB_8888)
    val androidCanvas = new AndroidCanvas(bitmap)
    drawable.draw(androidCanvas)
    val buffer = ByteBuffer.allocate(bitmap.getByteCount)
    bitmap.copyPixelsToBuffer(buffer)
    buffer.rewind()
    val imageInfo = new ImageInfo(width, height, ColorType.RGBA_8888, ColorAlphaType.PREMUL)
    Image.Companion.makeRaster(imageInfo, buffer.array(), width * 4)

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