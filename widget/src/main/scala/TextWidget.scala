package me.katze.gui4s.widget

import library.{Empty, TextDraw, TextPlacement}
import stateful.{BiMonad, CatchEvents}

import cats.{Applicative, FlatMap, Monad}
import cats.syntax.all.*

def textWidget[
  Update[+_] : Applicative,
  Draw,
  Place[+_] : FlatMap,
  LeftComposition : Empty,
  TextPlacementMeta,
  TextStyle,
  Shaper,
]
    (using
      textDraw : TextDraw[Draw, TextPlacementMeta],
      textIsPlaceable : TextPlacement[Shaper, TextStyle, Place[TextPlacementMeta]])
    (
      drawOnlyWidget : (Place[Widget[Update, Draw, Place, LeftComposition, Any]], Draw) => Widget[Update, Draw, Place, LeftComposition, Any],
      text : String,
      shaper : Shaper,
      style : TextStyle
    ) : Place[Widget[Update, Draw, Place, LeftComposition, Any]] =
  textIsPlaceable.sizeText(text, shaper, style).map:
    placementMetadata =>
      drawOnlyWidget(textWidget(drawOnlyWidget, text, shaper, style), textDraw.drawString(text, placementMetadata))
end textWidget
