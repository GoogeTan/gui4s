package gui4s.desktop.skija

import catnip.ForeignFunctionInterface
import cats.effect.{Resource, Sync}
import io.github.humbleui.skija.DirectContext

object DirectContext:
  def flush[F[_] : {ForeignFunctionInterface as ffi}](context : DirectContext) : F[Unit] =
    ffi(context.flush())
  end flush

  def createDirectContext[F[_] : {Sync, ForeignFunctionInterface as ffi}]: Resource[F, DirectContext] =
    Resource.fromAutoCloseable(
      ffi.delay(io.github.humbleui.skija.DirectContext.makeGL())
    )
  end createDirectContext
end DirectContext