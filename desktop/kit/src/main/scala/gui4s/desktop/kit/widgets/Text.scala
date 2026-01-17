package gui4s.desktop.kit
package widgets

import cats.*
import cats.effect.kernel.Sync
import gui4s.core.layout.*
import gui4s.desktop.kit.effects.*
import gui4s.desktop.kit.effects.Draw.given
import gui4s.desktop.kit.effects.Place.given
import gui4s.desktop.skija.*
import gui4s.desktop.skija.canvas.*
import gui4s.desktop.widget.library.text as genericText
import io.github.humbleui.skija.Paint
import io.github.humbleui.skija.paragraph.Paragraph
import io.github.humbleui.skija.shaper.Shaper

trait TextWidget[IO[_]]:
  def apply[Event](
    text : String,
    style : SkijaTextStyle
  ) : DesktopWidget[IO, Event]
end TextWidget

object TextWidget:
  def apply[IO[_] : Sync](
    shaper : Shaper,
    textCache : TextCache[IO],
  ) : TextWidget[IO] =
    new TextWidget[IO]:
      override def apply[Event](text : String, style : SkijaTextStyle) : DesktopWidget[IO, Event] =
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
      end apply
    end new
  end apply
end TextWidget

def placedText[IO[_] : Sync, Event](placedText : Sized[Float, SkijaPlacedText]) : DesktopWidget[IO, Event] =
  constSizedDrawOnlyWidget(
    placedText.mapValue(Draw.drawText)
  )
end placedText

def paragraph[IO[_] : Sync, Event](paragraph : Paragraph) : DesktopWidget[IO, Event] =
  OuterPlace.liftFunction(bounds =>
    sizeParagraph(paragraph, bounds.width.value)
  )
  .flatMap(placedParagraph)
end paragraph

def placedParagraph[IO[_] : Sync as S, Event](placedText : Sized[Float, Paragraph]) : DesktopWidget[IO, Event] =
  constSizedDrawOnlyWidget(
    placedText.mapValue(Draw.drawParagraph)
  )
end placedParagraph

def drawCursor[IO[_] : Sync, Event](paragraph: Sized[Float, Paragraph], cursor: Int, cursorPaint : Paint) : DesktopWidget[IO, Event] =
  constSizedDrawOnlyWidget(
     paragraph.mapValue(Draw.drawCursor(_, cursor, cursorPaint))
  )
end drawCursor
