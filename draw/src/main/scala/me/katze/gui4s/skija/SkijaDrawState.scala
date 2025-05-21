package me.katze.gui4s.skija

import catnip.syntax.all.{*, given}
import cats.*
import cats.data.ReaderT
import cats.syntax.all.*
import io.github.humbleui.skija.{Canvas, DirectContext}
import me.katze.gui4s.glfw.Glfw
import me.katze.gui4s.impure.Impure

final case class SkijaDrawState[F[_], Window](context : DirectContext, glfw: Glfw[F, Window], window : Window, canvas : Canvas)
type SkijaDraw[F[_], Window] = ReaderT[F, SkijaDrawState[F, Window], Unit]

given[F[_] : Applicative, Value]: Monoid[SkijaDraw[F, Value]] with
  override def empty: SkijaDraw[F, Value] = ReaderT.pure[F, SkijaDrawState[F, Value], Unit](())

  override def combine(x: SkijaDraw[F, Value], y: SkijaDraw[F, Value]): SkijaDraw[F, Value] =
    ReaderT[F, SkijaDrawState[F, Value], Unit]:
      state =>
        x(state) *> y(state)
  end combine
end given

def transition[F[_] : Impure as I](canvas : Canvas, x : Float, y : Float) : F[Unit] =
  I(canvas.translate(x, y))
end transition

def saveState[F[_] : Impure as I](canvas: Canvas) : F[Int] =
  I(canvas.save())
end saveState

def restoreState[F[_] : Impure as I](canvas: Canvas, state : Int) : F[Unit] =
  I(canvas.restoreToCount(state))
end restoreState

def moveAndBack[F[_] : {Impure as I, Monad}, T](canvas: Canvas, x : Float, y : Float, value : F[T]) : F[T] =
  for
    state <- saveState(canvas)
    _ <- transition(canvas, x, y)
    res <- value
    _ <- restoreState(canvas, state)
  yield res
end moveAndBack

def clear[F[_] : Impure as I](canvas: Canvas, color : Int) : F[Unit] =
   I(canvas.clear(color))
end clear

def flush[F[_] : Impure as I](context : DirectContext) : F[Unit] =
  I(context.flush())
end flush

def drawFrame[F[_] : {Monad, Impure as I}, Window](draw : SkijaDraw[F, Window]) : ReaderT[F, SkijaDrawState[F, Window], Unit] =
  ReaderT[F, SkijaDrawState[F, Window], Unit](state => 
    clear(state.canvas, 0xFFFFFFFF)
      |+| draw(state)
      |+| flush(state.context)
      |+| state.glfw.swapBuffers(state.window)
      |+| state.glfw.pollEvents
  )
end drawFrame

def flush[F[_] : {Monad, Impure as I}, Window]: ReaderT[F, SkijaDrawState[F, Window], Unit] =
  ReaderT[F, SkijaDrawState[F, Window], Unit](state =>
    flush(state.context)
      |+| state.glfw.swapBuffers(state.window)
      |+| state.glfw.pollEvents
      |+| clear(state.canvas, 0xFFFFFFFF)
  )
end flush





