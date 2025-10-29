package gui4s.desktop.skija
package typeface

import cats.effect.{Resource, Sync}
import io.github.humbleui.skija.Typeface

def defaultTypeface[F[_] : Sync as S] : Resource[F, Typeface] =
  Resource.fromAutoCloseable(
    S.delay(Typeface.makeDefault())
  )
end defaultTypeface





