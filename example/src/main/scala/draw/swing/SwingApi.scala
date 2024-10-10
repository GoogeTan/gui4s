package me.katze.gui4s.example
package draw.swing

import api.impl.DrawMonadT
import draw.{DrawApi, Window}

import cats.effect.IO

import java.awt.Frame
import javax.swing.JFrame
import scala.math.Numeric.Implicits.{*, given}

final class SwingApi[MU : Numeric](frame : JFrame, component : SwingWindowComponent) extends DrawApi[IO, MU]:
  override val window: Window[IO, MU] = SwingWindow(frame, component)

  override def graphics[Draw[_] : DrawMonadT[MU]](using Lift[IO, Draw, (MU, MU)]) =
    SwingDraw(component)
  end graphics
end SwingApi

object SwingApi:
  def invoke[MU : Numeric] : IO[DrawApi[IO, MU]] =
    IO:
      val frame = new JFrame("Image Drawing Component")
      frame.setSize(600, 600)
      val comp = new SwingWindowComponent()
      comp.setSize(600, 600)
      frame.add(comp)
      frame.setVisible(true)
      
      new SwingApi[MU](frame, comp)
  end invoke