package gui4s.desktop.kit
package common.widgets

import common.effects.*
import common.*
import common.effects.Place.given
import common.effects.Draw.given
import cats.*
import cats.effect.kernel.Sync
import cats.syntax.all.*
import gui4s.desktop.skija.*
import gui4s.desktop.skija.canvas.*
import gui4s.desktop.widget.library.{text as genericText, *}
import gui4s.core.layout.*
import io.github.humbleui.skija.shaper.Shaper
import io.github.humbleui.skija.paragraph.Paragraph

type TextWidget[IO[_], Event] =(
  text : String,
  style : SkijaTextStyle
) => DesktopWidget[IO, Event]

def text[IO[_] : Sync, Event](
  shaper : Shaper,
  textCache : TextCache[IO],
) : TextWidget[IO, Event] = 
  (text : String, style : SkijaTextStyle) =>
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
end text

def placedText[IO[_] : Sync, Event](placedText : Sized[Float, SkijaPlacedText]) : DesktopWidget[IO, Event] =
  drawOnlyWidget[
      UpdateC[IO, Event],
      PlaceC[IO],
      Draw[IO],
      RecompositionReaction[IO],
      DownEvent,
  ](
      OuterPlace.liftF(placedText.mapValue(Draw.drawText).pure[IO]),
      RecompositionReaction.empty
  )
end placedText

def placedParagraph[IO[_] : Sync, Event](placedText : Sized[Float, Paragraph]) : DesktopWidget[IO, Event] =
  drawOnlyWidget[
    UpdateC[IO, Event],
    PlaceC[IO],
    Draw[IO],
    RecompositionReaction[IO],
    DownEvent,
  ](
    OuterPlace.liftF(placedText.mapValue(Draw.drawParagraph).pure[IO]),
    RecompositionReaction.empty
  )
end placedParagraph
