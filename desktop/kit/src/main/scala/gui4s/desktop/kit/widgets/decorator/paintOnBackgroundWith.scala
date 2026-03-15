package gui4s.desktop.kit.widgets.decorator

import cats.effect._
import io.github.humbleui.skija.Path

import gui4s.core.geometry.Rect
import gui4s.core.layout.Sized

import gui4s.desktop.kit.effects._
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

  def paintOnBackgroundWith(brush: Brush, shape : Rect[Float] => Path): DesktopWidget[Event] =
    widget.withDrawOnlyBackground(
      PlacementEffect.getBounds.map:
        bounds =>
          val finiteBounds = bounds.map(_.getUnsafe)
          Sized(
            Draw.drawBrush(brush, finiteBounds, shape(finiteBounds)),
            finiteBounds,
          )
    )
  end paintOnBackgroundWith
end extension