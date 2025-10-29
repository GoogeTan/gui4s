package gui4s.desktop.kit
package widgets

import effects.*
import effects.Place.given

import cats.*
import gui4s.core.layout.*

def drawOnlyWidget[IO[_] : Monad, Event](
  draw : Place[IO, Draw[IO]]
) : DesktopWidget[IO, Event] =
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

def constSizedDrawOnlyWidget[IO[_] : Monad, Event](
  draw : Sized[Float, Draw[IO]]
) : DesktopWidget[IO, Event] =
  gui4s.desktop.widget.library.constanctSizeDrawOnlyWidget[
      UpdateC[IO, Event],
      OuterPlaceC[IO],
      InnerPlace,
      Draw[IO],
      RecompositionReaction[IO],
      DownEvent,
  ](
      draw,
      RecompositionReaction.empty
  )
end constSizedDrawOnlyWidget
