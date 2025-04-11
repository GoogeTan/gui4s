package me.katze.gui4s.widget
package library

type TextPlacementT[-Shaper, -TextStyle, PlacedText] = [F[_]] =>> TextPlacement[Shaper, TextStyle, F[PlacedText]]

trait TextPlacement[-Shaper, -TextStyle, +PlacedText]:
  def sizeText(text : String, shaper : Shaper, options : TextStyle) : PlacedText
end TextPlacement
