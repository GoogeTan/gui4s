package gui4s.desktop.skija
package font

import cats.effect.Resource
import cats.effect.kernel.Sync
import io.github.humbleui.skija.Font
import io.github.humbleui.skija.Typeface

def makeFont[F[_] : Sync as S](typeface : Typeface) : Resource[F, Font] =
  Resource.fromAutoCloseable(
    S.delay(new Font(typeface))
  )
end makeFont

def makeFont[F[_] : Sync as S](typeface : Typeface, size : Float) : Resource[F, Font] =
  Resource.fromAutoCloseable(
    S.delay(new Font(typeface, size))
  )
end makeFont
