package gui4s.desktop.kit.widgets.decorator

import cats.effect.*
import gui4s.core.layout.Sized
import gui4s.desktop.kit.effects.*
import gui4s.desktop.kit.widgets.DesktopWidget
import gui4s.desktop.skija.Brush

extension[Event](widget : DesktopWidget[Event])
  def paintOnBackgroundWith(brush : Brush) : DesktopWidget[Event] =
    widget.withDrawOnlyBackground(
      PlacementEffect.getBounds.map:
        bounds =>
          val finiteBounds = bounds.map(_.getUnsafe)
          Sized(
            Draw.drawBrush(brush, finiteBounds),
            finiteBounds,
          )
    )
  end paintOnBackgroundWith
end extension