package me.katze.gui4s.example
package api

trait DrawMonad[F, MeasurementUnit]:
  def drawAt(x : MeasurementUnit, y : MeasurementUnit, effect : F) : F
end DrawMonad
