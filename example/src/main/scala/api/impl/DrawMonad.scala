package me.katze.gui4s.example
package api.impl

type DrawMonadT[MeasurementUnit] = [F] =>> DrawMonad[F, MeasurementUnit]

trait DrawMonad[F, MeasurementUnit]:
  def move[T](dx : MeasurementUnit, dy : MeasurementUnit, effect : F) : F
end DrawMonad
