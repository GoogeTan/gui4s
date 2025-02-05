package me.katze.gui4s.example
package draw

import api.impl.DrawMonadT

trait DrawApi[F[_], MeasurementUnit : Numeric]:
  val window : Window[F, MeasurementUnit]
  def graphics[Draw[_] : DrawMonadT[MeasurementUnit]](using Lift[F, Draw, (MeasurementUnit, MeasurementUnit)]) : SimpleDrawApi[MeasurementUnit, Draw[Unit]]
end DrawApi

