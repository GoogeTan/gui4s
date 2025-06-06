package me.katze.gui4s.example
package api.exported

import me.*
import me.katze.gui4s.example.EventResult
import me.katze.gui4s.example.api.SkijaWidget_
import me.katze.gui4s.glfw.OglWindow
import me.katze.gui4s.layout.bound.Bounds
import me.katze.gui4s.layout.{Measurable, MeasurableT}
import me.katze.gui4s.skija.{*, given}

type Update[UpEvent] = [Value] =>> EventResult[Value, UpEvent]
type Recomposition[F[_]] = F[Unit]
type PlacedWidget[F[+_], +Event, -DownEvent] =  SkijaWidget_[[Value] =>> EventResult[Value, Event],  MeasurableT[F, Float], SkijaDraw[F, OglWindow], Recomposition[F], DownEvent]
type Widget[F[+_], +Event, -DownEvent] = Measurable[F, Float, PlacedWidget[F, Event, DownEvent]]