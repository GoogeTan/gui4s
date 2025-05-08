package me.katze.gui4s.example
package api

trait DrawMonad[F, MeasurementUnit]:
  def move(dx : MeasurementUnit, dy : MeasurementUnit, effect : F) : F
end DrawMonad
