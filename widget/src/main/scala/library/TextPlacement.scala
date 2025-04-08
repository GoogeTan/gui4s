package me.katze.gui4s.widget
package library

type TextPlacementT[PlacedText, -TextStyle] = [F[_]] =>> TextPlacement[F[PlacedText], TextStyle]

trait TextPlacement[+PlacedText, -TextStyle]:
  def sizeText(text : String, options : TextStyle) : PlacedText
end TextPlacement
