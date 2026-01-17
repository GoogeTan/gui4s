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

type TextWidget[IO[_]] = [Event] => (
  text : String,
  style : SkijaTextStyle
) => AndroidWidget[IO, Event]

def textWidget[IO[_] : Async](
  shaper : Shaper,
  textCache : gui4s.android.kit.TextCache[IO],
) : TextWidget[IO] =
  [Event] => (text : String, style : SkijaTextStyle) =>
    genericText[
      UpdateC[IO, Event],
      PlaceC[IO],
      Draw[IO],
      RecompositionReaction[IO],
      DownEvent,
      SkijaPlacedText
    ](
      Place.sizeText(shaper, textCache)(text, style),
      drawText,
      RecompositionReaction.empty,
    )
end textWidget

def placedText[IO[_] : Sync, Event](placedText : Sized[Float, SkijaPlacedText]) : AndroidWidget[IO, Event] =
  constSizedDrawOnlyWidget(
    placedText.mapValue(Draw.drawText)
  )
end placedText

def placedParagraph[IO[_] : Sync as S, Event](placedText : Sized[Float, Paragraph]) : AndroidWidget[IO, Event] =
  constSizedDrawOnlyWidget(
    placedText.mapValue(Draw.drawParagraph)
  )
end placedParagraph
