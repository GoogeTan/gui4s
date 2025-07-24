package me.katze.gui4s.skija

import io.github.humbleui.skija.*
import io.github.humbleui.skija.shaper.Shaper
import io.github.humbleui.types.Rect

import java.nio.file.{Files, Paths}

@main def drawTextExample(): Unit = {
  // Создаем поверхность для рисования
  val surface = Surface.makeRaster(ImageInfo.makeN32Premul(400, 100))
  val canvas = surface.getCanvas
  
  // Настраиваем параметры текста
  val paint = new Paint()
  paint.setColor(0xFF0000FF) // Синий цвет (ARGB)
  
  val font = new Font()
  font.setSize(32) // Размер шрифта
  
  // Создаем TextBlob из строки
  val text = "Привет, Skija!"
  val shaper = Shaper.make()
  val textBlob = shaper.shape(text, font)
  
  // Рисуем текст на канвасе
  canvas.drawRect(textBlob.getBounds, paint)
  paint.setColor(0xFFFF0000)
  canvas.drawTextBlob(textBlob, 0, 0, paint)
  val r = font.measureText(text, paint)
  canvas.translate(r.getWidth, r.getHeight)
  canvas.drawTextBlob(textBlob, 0, 0, paint)

  val image = surface.makeImageSnapshot()

  // Кодируем изображение в PNG
  val pngData = EncoderPNG.encode(image)

  // Сохраняем в файл
  val outputPath = Paths.get("skija-text.png")
  Files.write(outputPath, pngData.getBytes)

  // Очищаем ресурсы
  textBlob.close()
  font.close()
  paint.close()
  surface.close()
}
