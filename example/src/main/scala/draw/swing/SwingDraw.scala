package me.katze.gui4s.example
package draw.swing

import draw.{SimpleDrawApi, TextStyle}

import cats.effect.*

import java.awt.Color
import java.awt.image.BufferedImage
import scala.math.Numeric.Implicits.*
import scala.swing.Font

final class SwingDraw[
  Draw[_], 
  MU : Numeric
](
  canvas: SwingWindowComponent
)(using lift : Lift[IO, Draw, (MU, MU)]) extends SimpleDrawApi[MU, Draw[Unit]]:
  override def rectangle(x: MU, y: MU, width: MU, height: MU, color : Int): Draw[Unit] =
    lift.lift(zero =>
      IO:
        canvas.graphics.setColor(Color(color))
        canvas.graphics.fillRect((zero._1 + x).toInt, (zero._2 + y).toInt, width.toInt, height.toInt)
    )
  end rectangle
  
  
  override def text(x: MU, y: MU, text: String, style: TextStyle): Draw[Unit] =
    lift.lift((ox, oy) =>
      IO:
        val font = Font("Comis Sans MS", Font.Plain, style.size)
        canvas.graphics.setColor(Color(style.color))
        val h =  canvas.getFontMetrics(font).getStringBounds(text, canvas.graphics)
        canvas.setFont(font)
        canvas.graphics.drawString(text, (ox + x).toInt, (oy + y).toInt + (-h.getY).toInt)
    )
  end text
  
  override def endDraw: Draw[Unit] =
    lift.lift(_ =>
      IO:
        canvas.repaint()
    )
  end endDraw
  
  override def beginDraw: Draw[Unit] =
    lift.lift(_ =>
      IO:
        canvas.setImage(new BufferedImage(canvas.getWidth, canvas.getHeight, BufferedImage.TYPE_INT_ARGB))
    )
  end beginDraw
end SwingDraw
