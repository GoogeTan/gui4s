package me.katze.gui4s.example
package draw.swing

import draw.Window

import cats.effect.IO
import me.katze.gui4s.impure.Impure

import java.awt.Frame
import javax.swing.JFrame
import scala.math.Numeric.Implicits.*

final class SwingWindow[F[_], MU : Numeric](frame: JFrame, component: SwingWindowComponent, impure: Impure[F]) extends Window[F, MU]:
  override def size: F[(MU, MU)] =
    impure.impure:
      (Numeric[MU].fromInt(frame.getWidth), Numeric[MU].fromInt(frame.getHeight))
  end size

  override def resize(width: MU, height: MU): F[Unit] =
    impure.impure:
      frame.setExtendedState(Frame.NORMAL)
      frame.setUndecorated(false)
      frame.setSize(width.toInt, height.toInt)
      component.setSize(width.toInt, height.toInt)
  end resize

  override def enterFullScreen: F[Unit] =
    impure.impure:
      frame.setExtendedState(Frame.MAXIMIZED_BOTH)
      frame.setUndecorated(true)
      component.setSize(frame.getWidth, frame.getHeight)
  end enterFullScreen

  override def onResizedByUser: F[Unit] =
    impure.impure:
      ()
  end onResizedByUser
end SwingWindow
