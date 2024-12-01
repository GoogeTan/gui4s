package me.katze.gui4s.widget
package library

import stateful.{BiMonad, CatchEvents}

import cats.FlatMap
import cats.syntax.all.*

def labelWidget[
  Update[+_, +_] : BiMonad : CatchEvents,
  Draw,
  Place[+_] : FlatMap,
  LeftComposition : Empty,
  LabelPlacementMeta,
  TextStyle
]
    (using
        textDraw : LabelDraw[Draw, LabelPlacementMeta],
        textIsPlaceable : LabelPlacement[Place[LabelPlacementMeta], TextStyle])
    (
      drawOnlyWidget : (Place[Widget[Update, Draw, Place, LeftComposition, Nothing, Any]], Draw) => Widget[Update, Draw, Place, LeftComposition, Nothing, Any],
      text : String, 
      style : TextStyle
    ) : Place[Widget[Update, Draw, Place, LeftComposition, Nothing, Any]] =
  textIsPlaceable.sizeText(text, style).map:
    placementMetadata =>
      drawOnlyWidget(labelWidget(drawOnlyWidget, text, style), textDraw.drawString(text, placementMetadata))
end labelWidget
