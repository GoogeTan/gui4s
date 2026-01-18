package gui4s.desktop.skija
package typeface

import cats.effect.Resource
import cats.effect.Sync
import io.github.humbleui.skija.FontMgr
import io.github.humbleui.skija.Typeface

def defaultTypeface[F[_] : Sync as S] : Resource[F, Typeface] =
  Resource.fromAutoCloseable(
    S.delay(
      FontMgr.getDefault.makeFromFile("JetBrainsMono-Regular.ttf")
    )
  )
end defaultTypeface

def typefaceFromFile[F[_] : Sync as S](name: String) : Resource[F, Typeface] =
    Resource.fromAutoCloseable(
        S.delay(
            FontMgr.getDefault.makeFromFile(name)
        )
    )
end typefaceFromFile




