package me.katze.gui4s.example
package api

import api.impl.LayoutPlacementMeta

import cats.Functor
import me.katze.gui4s.widget
import me.katze.gui4s.widget.library.{Empty, TextDraw, TextPlacementT}
import me.katze.gui4s.widget.stateful.{BiMonad, given}
import me.katze.gui4s.widget.{drawOnlyWidget, textWidget as rawTextWidget}

type TextWidget[Widget, Shaper, TextStyle] = (text : String, shaper : Shaper, textStyle : TextStyle) => Widget

def textWidget[
  Update[+_, +_]: BiMonad,
  Draw,
  Place[+_] : {TextPlacementT[Shaper, TextStyle, LayoutPlacementMeta[MeasurementUnit]], Functor},
  Recomposition : Empty,
  MeasurementUnit,
  TextStyle,
  Shaper,
  SystemEvent,
](
   using TextDraw[Draw, LayoutPlacementMeta[MeasurementUnit]],
) : TextWidget[Place[widget.Widget[[W] =>> Update[W, Nothing], Draw, Place, Recomposition, SystemEvent]], Shaper, TextStyle] =
  (text: String, shaper: Shaper, style: TextStyle) => rawTextWidget(drawOnlyWidget, text, shaper, style)
end textWidget 
