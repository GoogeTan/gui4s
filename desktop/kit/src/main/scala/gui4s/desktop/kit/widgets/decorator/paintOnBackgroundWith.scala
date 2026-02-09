package gui4s.desktop.kit.widgets.decorator

import cats.effect.kernel.Sync
import gui4s.core.layout.Sized
import gui4s.desktop.kit.effects.*
import gui4s.desktop.kit.widgets.DesktopWidget
import gui4s.desktop.skija.Brush

extension[IO[_], Event](widget : DesktopWidget[IO, Event])
  def paintOnBackgroundWith(using Sync[IO])(brush : Brush) : DesktopWidget[IO, Event] =
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