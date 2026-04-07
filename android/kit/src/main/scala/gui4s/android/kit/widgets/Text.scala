package gui4s.android.kit.widgets

import gui4s.core.layout.*
import gui4s.android.kit.effects.*
import gui4s.android.kit.effects.Draw.given
import gui4s.android.kit.effects.Place.given
import gui4s.android.skia.*
import gui4s.android.skia.canvas.*
import gui4s.desktop.widget.library.text as genericText
import org.jetbrains.skia.paragraph.Paragraph
import org.jetbrains.skia.shaper.Shaper
import cats.effect.IO
import gui4s.core.layout.*
import gui4s.android.kit.effects.*
import gui4s.android.kit.effects.Draw.given
import gui4s.android.kit.effects.Place.given
import gui4s.android.skia.*
import gui4s.android.skia.canvas.*
import gui4s.core.geometry.Rect
import gui4s.desktop.widget.library.text as genericText
import org.jetbrains.skia.paragraph.Paragraph
import org.jetbrains.skia.shaper.Shaper

type TextWidget = [Event] => (
  text : String,
  style : SkijaTextStyle
) => AndroidWidget[Event]

def textWidget(
  shaper : Shaper,
  textCache : gui4s.android.kit.TextCache[IO],
) : TextWidget =
  [Event] => (text : String, style : SkijaTextStyle) =>
    genericText[
      UpdateC[Event],
      Place,
      Draw,
      RecompositionReaction,
      SkijaPlacedText
    ](
      Place.sizeText(shaper, textCache)(text, style),
      drawText,
      RecompositionReaction.empty,
    )
end textWidget

def placedText[Event](placedText : Sized[Rect[Float], SkijaPlacedText]) : AndroidWidget[Event] =
  constSizedDrawOnlyWidget(
    placedText.mapValue(Draw.drawText)
  )
end placedText

def placedParagraph[Event](placedText : Sized[Rect[Float], Paragraph]) : AndroidWidget[Event] =
  constSizedDrawOnlyWidget(
    placedText.mapValue(Draw.drawParagraph)
  )
end placedParagraph
