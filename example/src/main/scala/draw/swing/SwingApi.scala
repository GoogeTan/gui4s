package me.katze.gui4s.example
package draw.swing

import api.impl.DrawMonadT
import draw.{DrawApi, Window}

import cats.effect.Async
import cats.effect.kernel.Resource
import cats.effect.std.Dispatcher
import me.katze.gui4s.impure.Impure

import java.awt.event.*
import javax.swing.JFrame

final class SwingApi[F[_], MeasurementUnit : Numeric] private(
                                                               override val window: Window[F, MeasurementUnit],
                                                               component : SwingWindowComponent,
                                                               impure: Impure[F]
                                                ) extends DrawApi[F, MeasurementUnit]:
  

  override def graphics[Draw[_] : DrawMonadT[MeasurementUnit]](using Lift[F, Draw, (MeasurementUnit, MeasurementUnit)]) =
    SwingDraw(component, impure)
  end graphics
end SwingApi

object SwingApi:
  def invoke[F[_] : Async, MeasurementUnit : Numeric](window: (JFrame, SwingWindowComponent) => Window[F, MeasurementUnit], impure: Impure[F]) : Resource[F, DrawApi[F, MeasurementUnit]] =
    for
      dispatcher <- Dispatcher.sequential[F]
      (window, component) <- initResource(dispatcher, window, impure)
    yield SwingApi[F, MeasurementUnit](window, component, impure)
  end invoke
  
  def initResource[F[_], MeasurementUnit : Numeric](dispatcher: Dispatcher[F], window : (JFrame, SwingWindowComponent) => Window[F, MeasurementUnit], impure: Impure[F]) : Resource[F, (Window[F, MeasurementUnit], SwingWindowComponent)] =
    Resource.eval(init(dispatcher, window, impure))
  end initResource
  
  def init[F[_], MeasurementUnit : Numeric](dispatcher: Dispatcher[F], windowFabric : (JFrame, SwingWindowComponent) => Window[F, MeasurementUnit], impure: Impure[F]) : F[(Window[F, MeasurementUnit], SwingWindowComponent)] =
    impure.impure:
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
