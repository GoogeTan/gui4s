package me.katze.gui4s.skija

import catnip.ForeighFunctionInterface
import cats.Monad
import cats.data.ReaderT
import cats.syntax.all.*
import io.github.humbleui.skija.{Canvas, DirectContext}
import me.katze.gui4s.glfw.*
import me.katze.gui4s.glfw.GlfwWindow.*
import cats.Applicative
import cats.arrow.FunctionK

type SkijaDrawLoud[F[_], T] = ReaderT[F, Canvas, T]

object SkijaDrawLoud:
  def liftF[F[_], T](original : F[T]) : SkijaDrawLoud[F, T] =
    ReaderT.liftF(original)
  end liftF

  def getCanvas[F[_] : Applicative] : SkijaDrawLoud[F, Canvas] =
    ReaderT.ask
  end getCanvas

  def liftK[F[_] : Applicative] : FunctionK[F, SkijaDrawLoud[F, *]] =
    ReaderT.liftK
  end liftK
end SkijaDrawLoud

type SkijaDraw[F[_]] = SkijaDrawLoud[F, Unit]

def transition_[F[_]](ffi : ForeighFunctionInterface[F], canvas : Canvas, x : Float, y : Float) : F[Unit] =
  ffi(canvas.translate(x, y))
end transition_

def saveState_[F[_]](ffi : ForeighFunctionInterface[F], canvas: Canvas) : F[Int] =
  ffi(canvas.save())
end saveState_

def restoreState_[F[_]](ffi : ForeighFunctionInterface[F], canvas: Canvas, state : Int) : F[Unit] =
  ffi(canvas.restoreToCount(state))
end restoreState_

def moveAndBack_[F[_] : {Monad}, T](ffi : ForeighFunctionInterface[F], canvas: Canvas, x : Float, y : Float, value : F[T]) : F[T] =
  for
    state <- saveState_(ffi, canvas)
    _ <- transition_(ffi, canvas, x, y)
    res <- value
    _ <- restoreState_(ffi, canvas, state)
  yield res
end moveAndBack_

def clear_[F[_]](ffi : ForeighFunctionInterface[F], canvas: Canvas, color : Int) : F[Unit] =
  ffi(canvas.clear(color))
end clear_

def flush_[F[_]](ffi : ForeighFunctionInterface[F], context : DirectContext) : F[Unit] =
  ffi(context.flush())
end flush_

def drawAt[
  F[_] : Monad,
](
    ffi : ForeighFunctionInterface[F],
    original: SkijaDraw[F],
    x : Float,
    y : Float
): SkijaDraw[F] =
  ReaderT[F, Canvas, Unit](
    canvas =>
      moveAndBack_(
        ffi = ffi,
        canvas = canvas,
        x = x,
        y = y,
        value = original.run(canvas)
      )
  )
end drawAt

def drawText[F[_]](ffi : ForeighFunctionInterface[F], text: SkijaPlacedText) : SkijaDraw[F] =
  ReaderT[F, Canvas, Unit](
    canvas =>
      ffi:
        canvas.drawTextBlob(text.textBlob, 0, 0, text.paint)
  )
end drawText

def clear[
  F[_] : {Monad, ForeighFunctionInterface as I},
]: SkijaDraw[F] =
  ReaderT[F, Canvas, Unit](canvas => I(canvas.clear(0xFFFFFFFF)))
end clear
