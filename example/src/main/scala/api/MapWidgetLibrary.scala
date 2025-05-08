package me.katze.gui4s.example
package api

import cats.{Functor, Monoid}
import me.katze.gui4s.widget.{MapEventWidget as RawMapEventWidget, Widget}
import me.katze.gui4s.widget.stateful.BiMonad

type MapEventWidget[Widget[_]] = [A, B] => Widget[A] => (A => B) => Widget[B]

def mapWidget[
  Update[+_, +_] : BiMonad,
  Draw,
  Place[+_] : Functor,
  Recomposition : Monoid,
  DownEvent,
] : MapEventWidget[[Event] =>> Widget[[Value] =>> Update[Value, Event], Draw, Place, Recomposition, DownEvent]] =
  [A, B] => widget => f =>
    RawMapEventWidget(widget, f)
end mapWidget

extension[Widget[_], A](value : Widget[A])(using e : MapEventWidget[Widget])
  def mapEvent[B](f : A => B) : Widget[B] =
    e(value)(f)
