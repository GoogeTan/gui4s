package me.katze.gui4s.example
package api.impl

import api.EffectApi

import cats.Monad
import me.katze.gui4s.widget.PlacedWidget

trait EffectApiImpl[
  F[+_], 
  Update[+_, +_],
  Draw,
  Place[+_],
  WidgetTaskIn[+_],
  SystemEvent
] extends EffectApi[WidgetTaskIn]:
  override type Widget[+T] = Place[PlacedWidget[Update, Draw, Place, T, SystemEvent]]
  override type WidgetTask[+T] = WidgetTaskIn[T]

  override def sideEffect[T](name : String, task: WidgetTask[T]): Widget[T] =
    ???
  end sideEffect

  override def launchedEffect[T](name : String, keys: Any*)(task: WidgetTask[T]): Widget[T] =
    ???
  end launchedEffect
end EffectApiImpl
