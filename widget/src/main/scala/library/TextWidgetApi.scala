package me.katze.gui4s.widget
package library

import cats.{Applicative, Functor}
import cats.syntax.all.*

import me.katze.gui4s.widget
import me.katze.gui4s.widget.drawonly.drawOnlyWidget
import me.katze.gui4s.widget.library.{TextDraw, TextPlacement, textWidget as rawTextWidget}
import me.katze.gui4s.widget.{BiMonad, Empty, Widget, given }

type TextWidget[Widget, Shaper, TextStyle] = (text : String, shaper : Shaper, textStyle : TextStyle) => Widget

trait TextPlacement[-Shaper, -TextStyle, +PlacedText]:
  def sizeText(text : String, shaper : Shaper, options : TextStyle) : PlacedText
end TextPlacement

trait TextDraw[+Draw, -TextPlacementMeta]:
  def drawString(text: String, meta: TextPlacementMeta): Draw
end TextDraw

def textWidget[
  Update[+_, +_]: BiMonad,
  Draw,
  Place[+_] : Functor,
  Recomposition : Empty,
  MeasurementUnit,
  TextStyle,
  TextPlacementMeta,
  Shaper,
  SystemEvent,
](
    using TextDraw[Draw, TextPlacementMeta],
      TextPlacement[Shaper, TextStyle, Place[TextPlacementMeta]]
) : TextWidget[Place[widget.Widget[[Value] =>> Update[Value, Nothing], Draw, Place, Recomposition, SystemEvent]], Shaper, TextStyle] =
  (text: String, shaper: Shaper, style: TextStyle) =>
    rawTextWidget(drawOnlyWidget, text, shaper, style)
end textWidget

def textWidget[
  Update[+_] : Applicative,
  Draw,
  Place[+_] : Functor,
  LeftComposition : Empty,
  TextPlacementMeta,
  TextStyle,
  Shaper,
]
(using
    textDraw : TextDraw[Draw, TextPlacementMeta],
    textIsPlaceable : TextPlacement[Shaper, TextStyle, Place[TextPlacementMeta]]
)(
  drawOnlyWidget : (Place[Widget[Update, Draw, Place, LeftComposition, Any]], Draw) => Widget[Update, Draw, Place, LeftComposition, Any],
  text : String,
  shaper : Shaper,
  style : TextStyle
) : Place[Widget[Update, Draw, Place, LeftComposition, Any]] =
  textIsPlaceable.sizeText(text, shaper, style).map:
    placementMetadata =>
      drawOnlyWidget(textWidget(drawOnlyWidget, text, shaper, style), textDraw.drawString(text, placementMetadata))
end textWidget
