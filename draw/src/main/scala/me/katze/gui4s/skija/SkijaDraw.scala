package me.katze.gui4s.skija

import catnip.syntax.all.{*, given}
import cats.Monad
import cats.data.ReaderT
import cats.syntax.all.*
import io.github.humbleui.skija.{Canvas, DirectContext}
import me.katze.gui4s.impure.FFI

type SkijaDraw[F[_], Window] = ReaderT[F, SkijaDrawState[F, Window], Unit]

def transition_[F[_]](ffi : FFI[F], canvas : Canvas, x : Float, y : Float) : F[Unit] =
  ffi(canvas.translate(x, y))
end transition_

def saveState_[F[_]](ffi : FFI[F], canvas: Canvas) : F[Int] =
  ffi(canvas.save())
end saveState_

def restoreState_[F[_]](ffi : FFI[F], canvas: Canvas, state : Int) : F[Unit] =
  ffi(canvas.restoreToCount(state))
end restoreState_

def moveAndBack_[F[_] : {Monad}, T](ffi : FFI[F], canvas: Canvas, x : Float, y : Float, value : F[T]) : F[T] =
  for
    state <- saveState_(ffi, canvas)
    _ <- transition_(ffi, canvas, x, y)
    res <- value
    _ <- restoreState_(ffi, canvas, state)
  yield res
end moveAndBack_

def clear_[F[_]](ffi : FFI[F], canvas: Canvas, color : Int) : F[Unit] =
  ffi(canvas.clear(color))
end clear_

def flush_[F[_]](ffi : FFI[F], context : DirectContext) : F[Unit] =
  ffi(context.flush())
end flush_


def drawAt[F[_] : {Monad}, Window](ffi : FFI[F], original: SkijaDraw[F, Window], x : Float, y : Float): SkijaDraw[F, Window] =
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

def drawText[F[_], Window](ffi : FFI[F], text: SkijaPlacedText):SkijaDraw[F, Window] =
  ReaderT[F, SkijaDrawState[F, Window], Unit](
    state =>
      ffi:
        state.canvas.drawTextBlob(text.textBlob, 0, 0, text.paint)
  )
end drawText

def flush[F[_] : {Monad, FFI as I}, Window]: SkijaDraw[F, Window] =
  ReaderT[F, SkijaDrawState[F, Window], Unit](state =>
    I(state.context.flush())
      *> state.glfw.swapBuffers(state.window)
      *> I(state.canvas.clear(0xFFFFFFFF))
  )
end flush

def drawFrame[F[_] : {Monad}, Window](ffi : FFI[F], draw : SkijaDraw[F, Window]) : ReaderT[F, SkijaDrawState[F, Window], Unit] =
  ReaderT[F, SkijaDrawState[F, Window], Unit](state =>
    clear_(ffi, state.canvas, 0xFFFFFFFF)
      |+| draw(state)
      |+| flush_(ffi, state.context)
      |+| state.glfw.swapBuffers(state.window)
      |+| state.glfw.pollEvents
  )
end drawFrame




