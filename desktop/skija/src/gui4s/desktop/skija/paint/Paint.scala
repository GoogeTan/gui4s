package gui4s.desktop.skija
package paint

import cats.effect.Resource
import cats.effect.kernel.Sync
import io.github.humbleui.skija.Paint

def make[F[_] : Sync as S] : Resource[F, Paint] =
  Resource.fromAutoCloseable(S.delay(new Paint()))
end make

def setColour[F[_] : Sync as S](color : Int)(paint : Paint) : F[Unit] =
  S.delay(paint.setColor(color))
end setColour

