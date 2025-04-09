package me.katze.gui4s.widget

import library.{Empty, TextDraw, TextPlacement}
import stateful.{BiMonad, CatchEvents}

import cats.FlatMap
import cats.syntax.all.*

def textWidget[
  Update[+_, +_] : {BiMonad, CatchEvents},
  Draw,
  Place[+_] : FlatMap,
  LeftComposition : Empty,
  TextPlacementMeta,
  TextStyle
]
    (using
      textDraw : TextDraw[Draw, TextPlacementMeta],
      textIsPlaceable : TextPlacement[Place[TextPlacementMeta], TextStyle])
    (
      drawOnlyWidget : (Place[Widget[Update, Draw, Place, LeftComposition, Nothing, Any]], Draw) => Widget[Update, Draw, Place, LeftComposition, Nothing, Any],
      text : String, 
      style : TextStyle
    ) : Place[Widget[Update, Draw, Place, LeftComposition, Nothing, Any]] =
  textIsPlaceable.sizeText(text, style).map:
    placementMetadata =>
      drawOnlyWidget(textWidget(drawOnlyWidget, text, style), textDraw.drawString(text, placementMetadata))
end textWidget
