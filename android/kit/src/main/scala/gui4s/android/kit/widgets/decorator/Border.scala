package gui4s.android.kit.widgets.decorator

import cats.effect.*
import gui4s.core.geometry.Rect
import gui4s.core.layout.Sized
import gui4s.android.kit.effects.*
import gui4s.android.kit.widgets.AndroidWidget

extension[Event](widget : AndroidWidget[Event])
  def border(
    shape : Rect[Float] => Clip,
    brush: Any, // Missing Brush in Android
    style : Any = ??? // Missing SkPaint in Android
  ) : AndroidWidget[Event] =
    widget.withDrawOnlyBackground(
      PlacementEffect.getBounds.map:
        bounds =>
          val finiteBounds = bounds.map(_.getUnsafe)
          Sized(
            ???, // Missing Draw.drawBorder in Android
            finiteBounds,
          )
    )
  end border
end extension
