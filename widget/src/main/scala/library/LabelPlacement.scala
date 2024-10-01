package me.katze.gui4s.widget
package library

type LabelPlacementT[LabelPlacementMeta, -TextStyle] = [F[_]] =>> LabelPlacement[F[LabelPlacementMeta], TextStyle]

trait LabelPlacement[+LabelPlacementMeta, -TextStyle]:
  def sizeText(text : String, options : TextStyle) : LabelPlacementMeta
end LabelPlacement
