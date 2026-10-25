package gui4s.desktop.skija
package font

import cats.effect.Resource
import cats.effect.Sync
import io.github.humbleui.skija.paragraph.FontCollection

def makeFontCollection[F[_] : Sync as S] : Resource[F, FontCollection] =
  Resource.fromAutoCloseable(
    S.delay(
      new FontCollection
    )
  )
end makeFontCollection

