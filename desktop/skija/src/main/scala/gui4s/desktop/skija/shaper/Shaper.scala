package gui4s.desktop.skija
package shaper

import cats.effect.{Resource, Sync}
import io.github.humbleui.skija.shaper.Shaper
import io.github.humbleui.skija.{Font, TextBlob}

def createShaper[F[_] : Sync as S]: Resource[F, Shaper] =
  Resource.fromAutoCloseable(S.delay(Shaper.make()))
end createShaper
