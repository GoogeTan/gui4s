package me.katze.gui4s.widget
package library

import me.katze.gui4s.widget.stateful.{BiMonad, CatchEvents}

import cats.FlatMap
import cats.syntax.all.{*, given}

def label[
  Update[+_, +_] : BiMonad : CatchEvents,
  Draw,
  Place[+_] : FlatMap,
  LabelPlacementMeta,
  TextStyle
]
    (using
        textDraw : LabelDraw[Draw, LabelPlacementMeta],
        textIsPlaceable : LabelPlacement[Place[LabelPlacementMeta], TextStyle],
        drawOnlyWidget: (Place[PlacedWidget[Update, Draw, Place, Nothing, Any]], Draw) => PlacedWidget[Update, Draw, Place, Nothing, Any])
    (text : String, style : TextStyle) : Place[PlacedWidget[Update, Draw, Place, Nothing, Any]] =
  textIsPlaceable.sizeText(text, style).map:
    placementMetadata =>
      drawOnlyWidget(label(text, style), textDraw.drawString(text, placementMetadata))
end label
