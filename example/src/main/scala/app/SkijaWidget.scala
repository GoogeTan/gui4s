package me.katze.gui4s.example
package app

import catnip.syntax.all.given
import me.*
import me.katze.gui4s.example.api.exported.{SkijaPlace, SkijaPlaceT, SkijaRecomposition, SkijaUpdateT}
import me.katze.gui4s.glfw.{GlfwWindow, OglGlfwWindow}
import me.katze.gui4s.skija.{*, given}
import me.katze.gui4s.widget.library.Widget

type SkijaPlacedWidget[F[_], MeasurementUnit, UpdateError, PlaceError, Event, DownEvent] =
  Widget[SkijaUpdateT[F, MeasurementUnit, UpdateError, Event], SkijaPlaceT[F, MeasurementUnit, PlaceError], SkijaDraw[F, OglGlfwWindow], SkijaRecomposition[F], DownEvent]
type SkijaWidget[F[_], MeasurementUnit, UpdateError, PlaceError, Event, DownEvent] =
  SkijaPlace[F, MeasurementUnit, PlaceError, SkijaPlacedWidget[F, MeasurementUnit, UpdateError, PlaceError, Event, DownEvent]]
