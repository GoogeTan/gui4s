package gui4s.desktop.skija
package typeface

import cats.effect.{Resource, Sync}
import io.github.humbleui.skija.{FontMgr, Typeface}

def defaultTypeface[F[_] : Sync as S] : Resource[F, Typeface] =
  Resource.fromAutoCloseable(
    S.delay(
      FontMgr.getDefault.makeFromFile("JetBrainsMono-Regular.ttf")
    )
  )
end defaultTypeface





