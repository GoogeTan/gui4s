package gui4s.android.kit.widgets.decorator

import cats.effect.*
import gui4s.core.layout.Sized
import gui4s.android.kit.effects.*
import gui4s.android.kit.widgets.AndroidWidget

extension[Event](widget : AndroidWidget[Event])
  def paintOnBackgroundWith(brush : Any) : AndroidWidget[Event] =
    widget.withDrawOnlyBackground(
      PlacementEffect.getBounds.map:
        bounds =>
          val finiteBounds = bounds.map(_.getUnsafe)
          Sized(
            ???, // Missing Draw.drawBrush in Android
            finiteBounds,
          )
    )
  end paintOnBackgroundWith
end extension
