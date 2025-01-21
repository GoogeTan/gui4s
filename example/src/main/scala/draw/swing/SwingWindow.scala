package me.katze.gui4s.example
package draw.swing

import draw.Window

import cats.effect.IO

import java.awt.Frame
import javax.swing.JFrame
import scala.math.Numeric.Implicits.*

final class SwingWindow[MU : Numeric](frame: JFrame, component: SwingWindowComponent) extends Window[IO, MU]:
  override def size: IO[(MU, MU)] =
    IO:
      (Numeric[MU].fromInt(frame.getWidth), Numeric[MU].fromInt(frame.getHeight))
  end size

  override def resize(width: MU, height: MU): IO[Unit] = 
    IO:
      frame.setExtendedState(Frame.NORMAL)
      frame.setUndecorated(false)
      frame.setSize(width.toInt, height.toInt)
      component.setSize(width.toInt, height.toInt)
  end resize

  override def enterFullScreen: IO[Unit] =
    IO:
      frame.setExtendedState(Frame.MAXIMIZED_BOTH)
      frame.setUndecorated(true)
      component.setSize(frame.getWidth, frame.getHeight)
  end enterFullScreen

  override def onResizedByUser: IO[Unit] =
    IO.unit
  end onResizedByUser
end SwingWindow
