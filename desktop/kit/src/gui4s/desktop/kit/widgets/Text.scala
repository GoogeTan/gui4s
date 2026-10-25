package gui4s.desktop.kit
package widgets

import cats.*
import cats.effect.*
import io.github.humbleui.skija.Paint
import io.github.humbleui.skija.paragraph.Paragraph
import io.github.humbleui.skija.shaper.Shaper

import gui4s.core.layout.*

import gui4s.desktop.kit.effects.*
import gui4s.desktop.kit.effects.Draw.given
import gui4s.desktop.kit.effects.Place.given
import gui4s.desktop.skija.*
import gui4s.desktop.skija.canvas.*
import gui4s.desktop.widget.library.text as genericText

trait TextWidget:
  def apply[Event](
    text : String,
    style : SkijaTextStyle
  ) : DesktopWidget[Event]
end TextWidget

object TextWidget:
  def apply(
    shaper : Shaper,
    textCache : TextCache[IO],
  ) : TextWidget =
    new TextWidget:
      override def apply[Event](text : String, style : SkijaTextStyle) : DesktopWidget[Event] =
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
      end apply
    end new
  end apply

  def apply() : Init[TextWidget] =
    Init.evalResource(
      for
        shaper <- createShaper[IO]
        cache : TextCache[IO] <- ScalacacheCache()
      yield apply(shaper, cache)
    )
  end apply
end TextWidget

def placedText[Event](placedText : Sized[gui4s.core.geometry.Rect[Float], SkijaPlacedText]) : DesktopWidget[Event] =
  constSizedDrawOnlyWidget(
    placedText.mapValue(Draw.drawText)
  )
end placedText

def paragraph[Event](paragraph : Paragraph) : DesktopWidget[Event] =
  PlacementEffect.liftFunction(bounds =>
    sizeParagraph(paragraph, bounds.width.value)
  )
  .flatMap(placedParagraph)
end paragraph

def placedParagraph[Event](placedText : Sized[gui4s.core.geometry.Rect[Float], Paragraph]) : DesktopWidget[Event] =
  constSizedDrawOnlyWidget(
    placedText.mapValue(Draw.drawParagraph)
  )
end placedParagraph

def drawCursor[Event](paragraph: Sized[gui4s.core.geometry.Rect[Float], Paragraph], cursor: Int, cursorPaint : Paint) : DesktopWidget[Event] =
  constSizedDrawOnlyWidget(
     paragraph.mapValue(Draw.drawCursor(_, cursor, cursorPaint))
  )
end drawCursor
