package me.katze.gui4s.example
package api

type DrawMonadT[MeasurementUnit] = [F] =>> DrawMonad[F, MeasurementUnit]

trait DrawMonad[F, MeasurementUnit]:
  def move(dx : MeasurementUnit, dy : MeasurementUnit, effect : F) : F
end DrawMonad
