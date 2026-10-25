package gui4s.desktop.kit
package widgets

import cats.*
import cats.effect.*

import gui4s.core.geometry.Rect
import gui4s.core.layout.*

import gui4s.desktop.kit.effects.*
import gui4s.desktop.kit.effects.Place.given

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
