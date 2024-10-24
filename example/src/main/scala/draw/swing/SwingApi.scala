package me.katze.gui4s.example
package draw.swing

import api.impl.DrawMonadT
import draw.{DrawApi, Window}

import cats.effect.IO
import cats.syntax.all.{*, given}
import cats.effect.kernel.Resource
import cats.effect.std.Dispatcher

import java.awt.event.*
import javax.swing.JFrame
import scala.math.Numeric.Implicits.{*, given}

final class SwingApi[MU : Numeric] private(
                                            override val window: Window[IO, MU], 
                                            component : SwingWindowComponent
                                          ) extends DrawApi[IO, MU]:
  

  override def graphics[Draw[_] : DrawMonadT[MU]](using Lift[IO, Draw, (MU, MU)]) =
    SwingDraw(component)
  end graphics
end SwingApi

object SwingApi:
  def invoke[MU : Numeric](window : (JFrame, SwingWindowComponent) => Window[IO, MU]) : Resource[IO, DrawApi[IO, MU]] =
    for
      dispatcher <- Dispatcher.sequential[IO]
      (window, component) <- initResource(dispatcher, window)
    yield SwingApi[MU](window, component)
  end invoke
  
  def initResource[MU : Numeric](dispatcher: Dispatcher[IO], window : (JFrame, SwingWindowComponent) => Window[IO, MU]) : Resource[IO, (Window[IO, MU], SwingWindowComponent)] =
    Resource.eval(init(dispatcher, window))
  end initResource
  
  def init[MU : Numeric](dispatcher: Dispatcher[IO], windowFabric : (JFrame, SwingWindowComponent) => Window[IO, MU]) : IO[(Window[IO, MU], SwingWindowComponent)] =
    IO:
      val frame = new JFrame("Image Drawing Component")
      frame.setSize(600, 600)
      val comp = new SwingWindowComponent()
      comp.setSize(600, 600)
      frame.add(comp)
      frame.setVisible(true)
      val window = windowFabric(frame, comp)
      frame.addComponentListener(
        new ComponentAdapter:
          override def componentResized(e : ComponentEvent) : Unit =
            val w = e.getComponent.getWidth
            val h = e.getComponent.getHeight
            frame.setSize(w, h)
            comp.setSize(w, h)
            dispatcher.unsafeRunAndForget(// TODO Проверить, чо это норм
              window.onResizedByUser
            )
          end componentResized

          override def componentMoved(e : ComponentEvent) : Unit =
            ()
          end componentMoved
      )
      (window, comp)
  end init
end SwingApi
