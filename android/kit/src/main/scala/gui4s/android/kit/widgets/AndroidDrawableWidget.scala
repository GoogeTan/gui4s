package gui4s.android.kit.widgets

import android.graphics.Rect
import android.graphics.drawable.Drawable
import gui4s.android.kit.AndroidMainThread
import gui4s.android.kit.effects.OuterPlace
import gui4s.android.kit.effects.Draw
import gui4s.core.layout.Sized

def androidDrawableWidget[
  IO[_] : Async as S,
  Event
](drawable : Drawable) : AndroidWidget[IO, Event] =
  drawOnlyWidget(
    OuterPlace.liftFunction(bounds =>
      S.delay {
        drawable.setBounds(
          0,
          0,
          bounds.width.value.getOrElse(1_000_000f).toInt,
          bounds.height.value.getOrElse(1_000_000f).toInt
        )
        val size = gui4s.core.geometry.Rect(
          drawable.getBounds.right.toFloat,
          drawable.getBounds.bottom.toFloat
        )
        Sized(
          Draw.drawAndroidDrawable(drawable), size
        )
      }.evalOn(AndroidMainThread)
    )
  )
