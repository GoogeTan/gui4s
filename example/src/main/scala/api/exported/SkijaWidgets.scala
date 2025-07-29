package me.katze.gui4s.example
package api.exported

import catnip.syntax.all.given
import me.*
import me.katze.gui4s.glfw.{GlfwWindow, OglGlfwWindow}
import me.katze.gui4s.skija.{*, given}
import me.katze.gui4s.widget.library.Widget


type SkijaRecomposition[F[_]] = F[Unit]

type SkijaPlacedWidget[F[_], MeasurementUnit, UpdateError, PlaceError, Event, DownEvent] =
  Widget[SkijaUpdateT[F, UpdateError, MeasurementUnit, Event], SkijaPlaceT[F, MeasurementUnit, PlaceError], SkijaDraw[F, GlfwWindow[F, Long, Float]], SkijaRecomposition[F], DownEvent]
type SkijaWidget[F[_], MeasurementUnit, UpdateError, PlaceError, Event, DownEvent] =
  SkijaPlace[F, MeasurementUnit, PlaceError, SkijaPlacedWidget[F, MeasurementUnit, UpdateError, PlaceError, Event, DownEvent]]
