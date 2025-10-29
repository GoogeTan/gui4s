package gui4s.desktop.skija
package shaper

import cats.effect.{Resource, Sync}
import io.github.humbleui.skija.shaper.Shaper
import io.github.humbleui.skija.{Font, TextBlob}

def createShaper[F[_] : Sync as S]: Resource[F, Shaper] =
  Resource.fromAutoCloseable(S.delay(Shaper.make()))
end createShaper

def shape[F[_] : Sync as S](shaper : Shaper, text : String, font : Font) : F[TextBlob] =
  S.delay(
    shaper.shape(text, font)
  )
end shape

def shape[F[_] : Sync as S](shaper : Shaper, text : String, font : Font, width : Float) : F[TextBlob] =
  S.delay(
    shaper.shape(text, font, width)
  )
end shape

def shape[F[_] : Sync](shaper : Shaper, text : String, font : Font, maybeWidth : Option[Float]) : F[TextBlob] =
  maybeWidth match
    case Some(width) =>
      shape(shaper, text, font, width)
    case None =>
      shape(shaper, text, font)
  end match
end shape