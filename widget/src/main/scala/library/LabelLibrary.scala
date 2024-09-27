package me.katze.gui4s.widget
package library

import cats.Monad

trait LabelLibrary[LabelPlacementMeta] extends DrawOnlyWidgetLibrary:
  given textDraw : LabelDraw[Draw, LabelPlacementMeta]
  given textIsPlaceable : LabelPlacement[PlacementEffect[LabelPlacementMeta]]

  final def label(text : String) : FreeWidget[Nothing, Any] =
    Monad[PlacementEffect].map(textIsPlaceable.sizeText(text)):
      placementMetadata =>
        drawOnlyWidget(label(text), textDraw.drawString(text, placementMetadata))
  end label
end LabelLibrary
