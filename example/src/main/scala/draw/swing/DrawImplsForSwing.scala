package me.katze.gui4s.example
package draw.swing

import api.impl.LayoutPlacementMeta
import draw.{SimpleDrawApi, TextStyle}
import impl.{*, given}

import cats.Monoid
import cats.effect.IO
import me.katze.gui4s.widget.library.{TextDraw, LayoutDraw}

given swingLayoutDraw[Draw : Monoid, MeasurementUnit : Numeric]: LayoutDraw[SwingDraw[IO, MeasurementUnit, Unit], LayoutPlacementMeta[MeasurementUnit]] =
  layoutDrawImpl[SwingDraw[IO, MeasurementUnit, Unit], MeasurementUnit]
end swingLayoutDraw

given swingTextDraw[MeasurementUnit](using api : SimpleDrawApi[MeasurementUnit, SwingDraw[IO, MeasurementUnit, Unit]]): TextDraw[SwingDraw[IO, MeasurementUnit, Unit], LayoutPlacementMeta[MeasurementUnit]] =
  (text, meta) =>
    api.text(meta.x, meta.y, text, TextStyle(18, 0, 400))
end swingTextDraw
