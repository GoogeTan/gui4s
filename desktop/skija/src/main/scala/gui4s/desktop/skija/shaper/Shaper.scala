package gui4s.desktop.skija
package shaper

import cats.effect.Resource
import cats.effect.Sync
import io.github.humbleui.skija.shaper.Shaper

def createShaper[F[_] : Sync as S]: Resource[F, Shaper] =
  Resource.fromAutoCloseable(S.delay(Shaper.make()))
end createShaper
