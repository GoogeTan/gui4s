package me.katze.gui4s.example
package draw.skija

import cats.data.ReaderT
import io.github.humbleui.skija.shaper.Shaper
import me.katze.gui4s.example.api.impl.LayoutPlacementMeta
import me.katze.gui4s.example.draw.TextStyle
import me.katze.gui4s.impure.Impure
import me.katze.gui4s.widget.library.TextPlacement

final case class SkijaPlacedText()

given skijaTextPlacement[F[_] : Impure as I]: TextPlacement[ReaderT[F, Shaper, LayoutPlacementMeta[Float]], TextStyle] with
  override def sizeText(text: String, options: TextStyle): ReaderT[F, Shaper, LayoutPlacementMeta[Float]] =
    ???
  end sizeText
end skijaTextPlacement

