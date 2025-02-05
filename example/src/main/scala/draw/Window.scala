package me.katze.gui4s.example
package draw


trait Window[+F[_], MeasurementUnit]:
  def size : F[(MeasurementUnit, MeasurementUnit)]
  def resize(width : MeasurementUnit, height : MeasurementUnit) : F[Unit]
  def enterFullScreen : F[Unit]
  def onResizedByUser : F[Unit]
end Window
