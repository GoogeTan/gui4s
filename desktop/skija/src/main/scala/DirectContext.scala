package gui4s.desktop.skija

import cats.effect.{Resource, Sync}
import io.github.humbleui.skija.DirectContext

object DirectContext:
  def flush[F[_] : Sync as S](context : DirectContext) : F[Unit] =
    S.delay(context.flush())
  end flush

  def createDirectContext[F[_] : Sync as S]: Resource[F, DirectContext] =
    Resource.fromAutoCloseable(
      S.delay(io.github.humbleui.skija.DirectContext.makeGL())
    )
  end createDirectContext
end DirectContext