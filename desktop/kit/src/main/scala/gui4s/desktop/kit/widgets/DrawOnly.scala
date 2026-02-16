package gui4s.desktop.kit
package widgets

import cats._
import cats.effect.*

import gui4s.core.layout._

import gui4s.desktop.kit.effects.Place.given
import gui4s.desktop.kit.effects._

def drawOnlyWidget[Event](
  draw : Place[IO, Draw[IO]]
) : DesktopWidget[Event] =
  gui4s.desktop.widget.library.drawOnlyWidget[
      UpdateC[IO, Event],
      PlaceC[IO],
      Draw[IO],
      RecompositionReaction[IO],
      DownEvent,
  ](
      draw,
      RecompositionReaction.empty
  )
end drawOnlyWidget

def constSizedDrawOnlyWidget[Event](
  draw : Sized[Float, Draw[IO]]
) : DesktopWidget[Event] =
  gui4s.desktop.widget.library.constanctSizeDrawOnlyWidget[
      UpdateC[IO, Event],
      PlacementEffectC[IO],
      Situated,
      Draw[IO],
      RecompositionReaction[IO],
      DownEvent,
  ](
      draw,
      RecompositionReaction.empty
  )
end constSizedDrawOnlyWidget
