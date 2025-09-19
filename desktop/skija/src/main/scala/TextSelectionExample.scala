package gui4s.desktop.skija

import io.github.humbleui.skija.*
import io.github.humbleui.skija.paragraph.*
import io.github.humbleui.skija.shaper.Shaper
import io.github.humbleui.types.Rect

import java.nio.file.{Files, Paths}

object TextSelectionExample:
  def main(args: Array[String]): Unit =
    // Assuming a Skija surface and canvas setup (e.g., via BackendRenderTarget or similar).
    // For brevity, this example focuses on the drawing logic. In a real app, you'd integrate this into a window or view.

    val surface = Surface.makeRaster(ImageInfo.makeN32Premul(300, 50))
    val canvas  = surface.getCanvas

    // Clear the canvas
    canvas.clear(0xFFFFAFAA) // White background

    // Define typeface and font size
    val typeface = Typeface.makeDefault
    val fontSize = 16f

    // Normal text style: black text, no background
    val normalStyle = new TextStyle()
      .setTypeface(typeface)
      .setFontSize(fontSize)
      .setForeground(new Paint().setColor(0xFF0000FF)) // Black

    // Selected text style: white text on blue background
    val selectedStyle = new TextStyle()
      .setTypeface(typeface)
      .setFontSize(fontSize)
      .setForeground(new Paint().setColor(0xFFAAFFFF)) // White
      .setBackground(new Paint().setColor(0xFF0000FF)) // Blue

    // Paragraph style (default)
    val paraStyle = new ParagraphStyle

    // Build the paragraph with mixed styles
    val builder = new ParagraphBuilder(paraStyle, new FontCollection)
    try
      builder.pushStyle(normalStyle)
      builder.addText("This is some example text in a ")

      builder.pushStyle(selectedStyle)
      builder.addText("text field with selection")

      builder.pushStyle(normalStyle)
      builder.addText(" highlighted.")

      val par = builder.build.paint(canvas, 0f, 0f)
      par.close()
      val snapshot = surface.makeImageSnapshot()
      val pngData  = EncoderPNG.encode(snapshot)
      Files.write(Paths.get("output.png"), pngData.getBytes())
    finally
      builder.close()
      surface.close()
