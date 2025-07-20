package me.katze.gui4s.skija

import catnip.ForeighFunctionInterface
import catnip.syntax.all.given
import cats.Monad
import cats.data.ReaderT
import cats.syntax.all.*
import io.github.humbleui.skija.{Canvas, DirectContext}

type SkijaDraw[F[_], Window] = ReaderT[F, SkijaDrawState[F, Window], Unit]

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


def drawAt[F[_] : {Monad}, Window](ffi : ForeighFunctionInterface[F], original: SkijaDraw[F, Window], x : Float, y : Float): SkijaDraw[F, Window] =
  ReaderT[F, SkijaDrawState[F, Window], Unit](
    state =>
      moveAndBack_(
        ffi = ffi,
        canvas = state.canvas,
        x = x,
        y = y,
        value = original.run(state)
      )
  )
end drawAt

def drawText[F[_], Window](ffi : ForeighFunctionInterface[F], text: SkijaPlacedText) : SkijaDraw[F, Window] =
  ReaderT[F, SkijaDrawState[F, Window], Unit](
    state =>
      ffi:
        state.canvas.drawTextBlob(text.textBlob, 0, 0, text.paint)
  )
end drawText

def flush[F[_] : {Monad, ForeighFunctionInterface as I}, Window <: me.katze.gui4s.glfw.Window[F, Monitor], Monitor]: SkijaDraw[F, Window] =
  ReaderT[F, SkijaDrawState[F, Window], Unit](state =>
    I(state.context.flush())
      *> state.window.swapBuffers
      *> I(state.canvas.clear(0xFFFFFFFF))
  )
end flush
