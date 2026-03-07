package gui4s.android.kit.widgets

import cats.effect.IO
import gui4s.core.layout.*
import gui4s.android.kit.effects.*
import gui4s.android.kit.effects.Place.given

def drawOnlyWidget[Event](
  draw : Place[Draw]
) : AndroidWidget[Event] =
  gui4s.desktop.widget.library.drawOnlyWidget[
      UpdateC[Event],
      PlaceC,
      Draw,
      RecompositionReaction,
      DownEvent,
  ](
      draw,
      RecompositionReaction.empty
  )
end drawOnlyWidget

def constSizedDrawOnlyWidget[Event](
  draw : Sized[Float, Draw]
) : AndroidWidget[Event] =
  gui4s.desktop.widget.library.constanctSizeDrawOnlyWidget[
      UpdateC[Event],
      PlacementEffectC,
      Situated,
      Draw,
      RecompositionReaction,
      DownEvent,
  ](
      draw,
      RecompositionReaction.empty
  )
end constSizedDrawOnlyWidget

/*
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
end androidDrawableWidget*/