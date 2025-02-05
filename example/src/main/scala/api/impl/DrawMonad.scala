package me.katze.gui4s.example
package api.impl

type DrawMonadT[MeasurementUnit] = [F[_]] =>> DrawMonad[F, MeasurementUnit]

trait DrawMonad[F[_], MeasurementUnit]:
  def move[T](dx : MeasurementUnit, dy : MeasurementUnit, effect : F[T]) : F[T]
end DrawMonad
