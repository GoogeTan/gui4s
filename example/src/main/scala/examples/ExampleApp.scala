package me.katze.gui4s.example
package examples

import api.effects.{SkijaApplicationRequest, SkijaDownEvent}
import app.skijaGlfwCatsApp
import skija.SkijaBackend

import cats.effect.{ExitCode, IO, IOApp}
import me.katze.gui4s.geometry.Rect
import me.katze.gui4s.glfw.{OglGlfwWindow, WindowCreationSettings}
import me.katze.gui4s.widget.library

trait ExampleApp:
  type Update[Event, Value]
  final type UpdateC[Event] = [Value] =>> Update[Event, Value]
  type Place[Event]
  type Draw
  type RecompositionReaction
  type DownEvent

  type PlacedWidget[Event] = library.Widget[UpdateC[Event], Place, Draw, RecompositionReaction, Event]
  type Widget[Event] = Place[PlacedWidget[Event]]
end ExampleApp

