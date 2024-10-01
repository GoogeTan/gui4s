package me.katze.gui4s.example
package draw

import api.impl.DrawMonadT

import cats.effect.IO

import java.awt.Frame
import javax.swing.JFrame

trait SwingApi[F[_]]:
  def setSize(option: Option[(Int, Int)]) : F[Unit]
  def size : F[(Int, Int)]
  def graphics[Draw[_] : DrawMonadT[MU], MU : Numeric](using Lift[IO, Draw, (MU, MU)]) : SimpleDrawApi[MU, Draw[Unit]]
end SwingApi

object SwingApi:
  def invoke : IO[SwingApi[IO]] =
    IO:
      val frame = new JFrame("Image Drawing Component")
      frame.setSize(400, 400)
      val comp = new SwingWindowComponent()
      comp.setSize(400, 400)
      frame.add(comp)
      frame.setVisible(true)


      new SwingApi[IO]:
        override def setSize(option: Option[(Int, Int)]): IO[Unit] =
          option match
            case Some((w, h)) =>
              IO:
                frame.setExtendedState(Frame.NORMAL)
                frame.setUndecorated(false)
                frame.setSize(w, h)
                comp.setSize(w, h)
            case None =>
              IO:
                frame.setExtendedState(Frame.MAXIMIZED_BOTH)
                frame.setUndecorated(true)
          end match
        end setSize

        override def graphics[Draw[_] : DrawMonadT[MU], MU: Numeric](using Lift[IO, Draw, (MU, MU)]) =
          SwingDraw(comp)
        end graphics

        override def size: IO[(Int, Int)] =
          IO:
            (frame.getWidth, frame.getHeight)
        end size
      end new
  end invoke
end SwingApi
