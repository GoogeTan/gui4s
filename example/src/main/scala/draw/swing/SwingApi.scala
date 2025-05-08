package me.katze.gui4s.example
package draw.swing

import api.impl.DrawMonadT
import draw.DrawApi

import cats.effect.Async
import cats.effect.kernel.Resource
import cats.effect.std.Dispatcher
import me.katze.gui4s.impure.Impure
import me.katze.gui4s.layout.bound.Bounds

import java.awt.event.*
import javax.swing.JFrame

def SwingApi[
              F[_] : Async, 
              MeasurementUnit : Numeric, 
              Draw : DrawMonadT[MeasurementUnit]
            ](
              impure: Impure[F], 
              onResized : F[Unit],
              lift : (SwingDrawState[MeasurementUnit] => F[Unit]) => Draw
            ): Resource[F, DrawApi[F, MeasurementUnit, Draw]] =
  for
    dispatcher <- Dispatcher.sequential[F]
    component <- initResource(dispatcher, impure, onResized)
  yield DrawApi[F, MeasurementUnit, Draw](
    impure.impure:
      new Bounds(
        Numeric[MeasurementUnit].fromInt(component.getWidth),
        Numeric[MeasurementUnit].fromInt(component.getHeight)
      ),
    new SwingSimpleDrawApi(component, impure, lift)
  )
end SwingApi

def initResource[F[_], MeasurementUnit : Numeric](dispatcher: Dispatcher[F], impure: Impure[F], onResized : F[Unit]) : Resource[F, SwingWindowComponent] =
  Resource.eval(
    impure:
      init(() => dispatcher.unsafeRunAndForget(onResized))// TODO Проверить, чо это норм
  )
end initResource

def init[MeasurementUnit : Numeric](onResized : () => Unit) : SwingWindowComponent =
  val frame = new JFrame("Image Drawing Component")
  frame.setSize(600, 600)
  val comp = new SwingWindowComponent()
  comp.setSize(600, 600)
  frame.add(comp)
  frame.setVisible(true)
  frame.addComponentListener(
    new ComponentAdapter:
      override def componentResized(e : ComponentEvent) : Unit =
        val width = e.getComponent.getWidth
        val height = e.getComponent.getHeight
        frame.setSize(width, height)
        comp.setSize(width, height)
        onResized()
      end componentResized

      override def componentMoved(e : ComponentEvent) : Unit =
        ()
      end componentMoved
  )
  comp
end init
