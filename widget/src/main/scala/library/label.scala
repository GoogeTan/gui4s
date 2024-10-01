package me.katze.gui4s.widget
package library

import library.lowlevel.WidgetLibrary

def label[LabelPlacementMeta, TextStyle]
    (using
        lib : WidgetLibrary,
        textDraw : LabelDraw[lib.Draw, LabelPlacementMeta],
        textIsPlaceable : LabelPlacement[lib.PlacementEffect[LabelPlacementMeta], TextStyle],
        drawOnlyWidget: (lib.FreeWidget[Nothing, Any], lib.Draw) => lib.PlacedWidget[Nothing, Any])
    (text : String, style : TextStyle) : lib.FreeWidget[Nothing, Any] =
  lib.placementIsEffect.map(textIsPlaceable.sizeText(text, style)):
    placementMetadata =>
      drawOnlyWidget(label(text, style), textDraw.drawString(text, placementMetadata))
end label
