package gui4s.desktop.skija

import io.github.humbleui.skija.*
import io.github.humbleui.types.Rect

import java.nio.file.{Files, Paths}

object RenderImage:
  def main(args: Array[String]): Unit =
    val inPath  = if args.nonEmpty then args(0) else "sticker.webp"
    val outPath = if args.length > 1 then args(1) else "output.png"

    // Размер итогового изображения (холста)
    val width  = 800
    val height = 600

    // Создаём растровую поверхность RGBA premultiplied
    val surface = Surface.makeRasterN32Premul(width, height)
    val canvas  = surface.getCanvas

    // Заливаем фон
    canvas.clear(Color.makeRGB(250, 250, 250))

    // Загружаем входную картинку
    val data = Data.makeFromFileName(inPath)
    val img  = Image.makeDeferredFromEncodedBytes(data.getBytes)
    data.close()

    // Пэйнт со сглаживанием
    val paint = Paint().setAntiAlias(true)

    // Простейшая композиция: трансформации и рисование картинки с линейной фильтрацией
    canvas.save()
    canvas.translate(width / 2f, height / 2f) // центр холста
    canvas.rotate(20)                          // поворот на 20°
    canvas.scale(0.75f, 0.75f)                 // масштаб 75%

    // Рисуем изображение в центре, используя SamplingMode.LINEAR для плавного масштабирования
    val dst = Rect.makeXYWH(
      -img.getWidth  / 2f,
      -img.getHeight / 2f,
      img.getWidth.toFloat,
      img.getHeight.toFloat
    )
    canvas.drawImageRect(img, Rect.makeWH(img.getWidth, img.getHeight), dst, SamplingMode.LINEAR, paint, true)
    canvas.restore()

    // Рамка по краю
    val border = Paint()
      .setAntiAlias(true)
      .setColor(Color.makeRGB(30, 30, 30))
      .setMode(PaintMode.STROKE)
      .setStrokeWidth(2)
    canvas.drawRect(Rect.makeXYWH(10, 10, width - 20, height - 20), border)

    // Снимок и сохранение в PNG
    val snapshot = surface.makeImageSnapshot()
    val pngData  = EncoderPNG.encode(snapshot)
    Files.write(Paths.get(outPath), pngData.getBytes())

    // Освобождение ресурсов
    pngData.close()
    snapshot.close()
    border.close()
    paint.close()
    img.close()
    surface.close()
