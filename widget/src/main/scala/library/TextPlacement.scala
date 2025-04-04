package me.katze.gui4s.widget
package library

type TextPlacementT[LabelPlacementMeta, -TextStyle] = [F[_]] =>> TextPlacement[F[LabelPlacementMeta], TextStyle]

trait TextPlacement[+LabelPlacementMeta, -TextStyle]:
  def sizeText(text : String, options : TextStyle) : LabelPlacementMeta
end TextPlacement
