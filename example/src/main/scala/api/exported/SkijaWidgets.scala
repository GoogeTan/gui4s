package me.katze.gui4s.example
package api.exported

import catnip.syntax.all.given
import me.*
import me.katze.gui4s.glfw.OglWindow
import me.katze.gui4s.skija.{*, given}
import me.katze.gui4s.widget.library.Widget_


type SkijaRecomposition[F[_]] = F[Unit]

type SkijaPlacedWidget[F[_], MeasurementUnit, PlaceError, Event, DownEvent] = 
  Widget_[SkijaUpdateT[F, MeasurementUnit, Event], SkijaPlaceT[F, MeasurementUnit, PlaceError], SkijaDraw[F, OglWindow], SkijaRecomposition[F], DownEvent]
type SkijaWidget[F[_], MeasurementUnit, PlaceError, Event, DownEvent] =
  SkijaPlace[F, MeasurementUnit, PlaceError, SkijaPlacedWidget[F, MeasurementUnit, PlaceError, Event, DownEvent]]
