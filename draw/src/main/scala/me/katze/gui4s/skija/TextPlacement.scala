package me.katze.gui4s.skija

import cats.data.ReaderT
import io.github.humbleui.skija.{Font, TextBlob}
import io.github.humbleui.skija.shaper.Shaper
import me.katze.gui4s.impure.Impure

def placeText[F[_] : Impure as I](shaper: Shaper, availablePlaceHorizontal : Float, text : String, font : Font) : F[TextBlob] =
  I:
    shaper.shape(text, font, availablePlaceHorizontal)
end placeText

trait TextPlacementContext[MU]:
  def availableWidth : MU
  def shaper : Shaper
end TextPlacementContext

def placeText[F[_] : Impure](text : String, font: Font) : ReaderT[F, TextPlacementContext[Float], TextBlob] =
  ReaderT(
    ctx =>
      placeText(
        ctx.shaper,
        ctx.availableWidth,
        text,
        font
      )
  )
end placeText
