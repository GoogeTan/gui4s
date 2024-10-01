package me.katze.gui4s.example
package draw

import java.awt.Graphics
import java.awt.image.BufferedImage
import javax.swing.JComponent

class SwingWindowComponent extends JComponent:
  private[draw] var image: BufferedImage = new BufferedImage(400, 400, BufferedImage.TYPE_INT_ARGB)
  private[draw] var graphics = image.createGraphics()

  def setImage(newImage : BufferedImage) : Unit =
    image = newImage
    graphics = newImage.createGraphics()
  end setImage

  override def paintComponent(g: Graphics): Unit =
    super.paintComponent(g)
    g.drawImage(image, 0, 0, null)
    graphics.drawImage(image, 0, 0, null)
  end paintComponent
end SwingWindowComponent
