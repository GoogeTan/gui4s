package gui4s.desktop.skija

import io.github.humbleui.skija.*
import io.github.humbleui.skija.paragraph.*

import java.nio.file.{Files, Paths}

object TextSelectionExample:
  def main(args: Array[String]): Unit =

    val surface = Surface.makeRaster(ImageInfo.makeN32Premul(500, 300))
    val canvas  = surface.getCanvas

    // Clear the canvas
    canvas.clear(Color.makeRGB(255, 255, 255)) // White background

    val fontCollection = new FontCollection
    fontCollection.setDefaultFontManager(FontMgr.getDefault)

    // Define typeface and font size
    val typeface = Typeface.makeDefault
    val fontSize = 32f

    // Normal text style: black text, no background
    val normalStyle = new TextStyle()
      .setTypeface(typeface)
      .setFontSize(fontSize)
      .setForeground(new Paint().setColor(0xFF000000)) // Black

    // Selected text style: white text on blue background
    val selectedStyle = new TextStyle()
      .setTypeface(typeface)
      .setFontSize(fontSize)
      .setForeground(new Paint().setColor(0xFF000000)) // White
      .setBackground(new Paint().setColor(0xFF486a97)) // Blue

    // Paragraph style (default)
    val paraStyle = new ParagraphStyle

    // Build the paragraph with mixed styles
    val builder = new ParagraphBuilder(paraStyle, fontCollection)
    try
      builder.pushStyle(normalStyle)
      builder.addText("This is some example text in a ")

      builder.pushStyle(selectedStyle)
      builder.addText("text field with selection")

      builder.pushStyle(normalStyle)
      builder.addText(" highlighted.")

      builder.build.layout(300f).paint(canvas, 0f, 0f).close()
      val snapshot = surface.makeImageSnapshot()
      val pngData  = EncoderPNG.encode(snapshot)
      Files.write(Paths.get("output.png"), pngData.getBytes())
    finally
      builder.close()
      surface.close()
  end main
end TextSelectionExample