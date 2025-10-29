package gui4s.desktop.skija
package font

import cats.effect.{Resource, Sync}
import io.github.humbleui.skija.FontMgr

def makeFontMgr[F[_] : Sync as S] : Resource[F, FontMgr] =
  Resource.fromAutoCloseable(
    S.delay(
      FontMgr.getDefault
    )
  )
end makeFontMgr
