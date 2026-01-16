package gui4s.android.kit.widgets

import android.graphics.drawable.Drawable
import gui4s.core.layout.*
import gui4s.android.kit.effects.*
import gui4s.android.kit.effects.Place.given
import gui4s.core.geometry.Rect
import cats.effect.syntax.all.*

def drawOnlyWidget[IO[_] : Monad, Event](
  draw : Place[IO, Draw[IO]]
) : AndroidWidget[IO, Event] =
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
) : AndroidWidget[IO, Event] =
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

def androidDrawableWidget[
  IO[_] : Async,
  Event
](
  image: Drawable,
): AndroidWidget[IO, Event] =
  constSizedDrawOnlyWidget[
    IO,
    Event
  ](
    Sized(Draw.drawAndroidDrawable(image).evalOn(gui4s.android.kit.AndroidMainThread), Rect(image.getIntrinsicWidth.toFloat, image.getIntrinsicHeight.toFloat)),
  )
end androidDrawableWidget