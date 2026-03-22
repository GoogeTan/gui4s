package gui4s.desktop.kit
package widgets

import cats._
import cats.effect._

import gui4s.core.geometry.Rect
import gui4s.core.layout._

import gui4s.desktop.kit.effects.Place.given
import gui4s.desktop.kit.effects._

def drawOnlyWidget[Event](
  draw : Place[Draw]
) : DesktopWidget[Event] =
  gui4s.desktop.widget.library.drawOnlyWidget[
      UpdateC[Event],
      Place,
      Draw,
      RecompositionReaction,
  ](
      draw,
      RecompositionReaction.empty
  )
end drawOnlyWidget

def constSizedDrawOnlyWidget[Event](
  draw : Sized[Rect[Float], Draw]
) : DesktopWidget[Event] =
  gui4s.desktop.widget.library.constanctSizeDrawOnlyWidget[
      UpdateC[Event],
      PlacementEffect,
      Situated,
      Draw,
      RecompositionReaction,
  ](
      draw,
      RecompositionReaction.empty
  )
end constSizedDrawOnlyWidget
