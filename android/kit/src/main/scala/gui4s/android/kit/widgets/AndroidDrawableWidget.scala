package gui4s.android.kit.widgets

import android.graphics.drawable.Drawable
import cats.effect.IO
import gui4s.android.kit.AndroidMainThread
import gui4s.android.kit.effects.PlacementEffect
import gui4s.android.kit.effects.Draw
import gui4s.core.layout.Sized

def androidDrawableWidget[
  Event
](drawable : Drawable) : AndroidWidget[Event] =
  drawOnlyWidget(
    PlacementEffect.liftFunction(bounds =>
      IO.delay {
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
