package me.katze.gui4s.example
package draw.swing

import draw.{SimpleDrawApi, TextStyle}

import me.katze.gui4s.impure.Impure

import java.awt.Color
import java.awt.image.BufferedImage
import scala.math.Numeric.Implicits.*
import scala.swing.Font

final class SwingSimpleDrawApi[
  F[_],
  Draw,
  MeasurementUnit : Numeric
](
  canvas: SwingWindowComponent,
  impure: Impure[F],
  lift : (SwingDrawState[MeasurementUnit] => F[Unit]) => Draw
) extends SimpleDrawApi[MeasurementUnit, Draw]:
  override def rectangle(x: MeasurementUnit, y: MeasurementUnit, width: MeasurementUnit, height: MeasurementUnit, color : Int): Draw =
    lift(state =>
      impure.impure:
        canvas.graphics.setColor(Color(color))
        canvas.graphics.fillRect((state.x + x).toInt, (state.y + y).toInt, width.toInt, height.toInt)
    )
  end rectangle
  
  
  override def text(x: MeasurementUnit, y: MeasurementUnit, text: String, style: TextStyle): Draw =
    lift(state =>
      impure.impure:
        val font = Font("Comis Sans MS", Font.Plain, style.size)
        canvas.graphics.setColor(Color(style.color))
        val h =  canvas.getFontMetrics(font).getStringBounds(text, canvas.graphics)
        canvas.setFont(font)
        canvas.graphics.drawString(text, (state.x + x).toInt, (state.y + y).toInt + (-h.getY).toInt)
    )
  end text
  
  override def endDraw: Draw =
    lift(_ =>
      impure.impure:
        canvas.repaint()
    )
  end endDraw
  
  override def beginDraw: Draw =
    lift(_ =>
      impure.impure:
        canvas.setImage(new BufferedImage(canvas.getWidth, canvas.getHeight, BufferedImage.TYPE_INT_ARGB))
    )
  end beginDraw
end SwingSimpleDrawApi

