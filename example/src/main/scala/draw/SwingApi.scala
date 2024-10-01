package me.katze.gui4s.example
package draw

import api.impl.DrawMonadT

import cats.effect.IO

import java.awt.Frame
import javax.swing.JFrame

trait SwingApi[F[_], MU : Numeric]:
  def setSize(option: Option[(MU, MU)]) : F[Unit]
  def size : F[(MU, MU)]
  def graphics[Draw[_] : DrawMonadT[MU]](using Lift[IO, Draw, (MU, MU)]) : SimpleDrawApi[MU, Draw[Unit]]
end SwingApi

object SwingApi:
  def invoke[MU : Numeric] : IO[SwingApi[IO, MU]] =
    IO:
      val frame = new JFrame("Image Drawing Component")
      frame.setSize(600, 600)
      val comp = new SwingWindowComponent()
      comp.setSize(600, 600)
      frame.add(comp)
      frame.setVisible(true)


      new SwingApi[IO, MU]:
        override def setSize(option: Option[(MU, MU)]): IO[Unit] =
          option match // TODO notify draw thread about changes
            case Some((w, h)) =>
              IO:
                frame.setExtendedState(Frame.NORMAL)
                frame.setUndecorated(false)
                frame.setSize(Numeric[MU].toInt(w), Numeric[MU].toInt(h))
                comp.setSize(Numeric[MU].toInt(w), Numeric[MU].toInt(h))
            case None =>
              IO:
                frame.setExtendedState(Frame.MAXIMIZED_BOTH)
                frame.setUndecorated(true)
          end match
        end setSize

        override def graphics[Draw[_] : DrawMonadT[MU]](using Lift[IO, Draw, (MU, MU)]) =
          SwingDraw(comp)
        end graphics

        override def size: IO[(MU, MU)] =
          IO:
            (Numeric[MU].fromInt(frame.getWidth), Numeric[MU].fromInt(frame.getHeight))
        end size
      end new
  end invoke
end SwingApi
