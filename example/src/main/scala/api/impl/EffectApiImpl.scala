package me.katze.gui4s.example
package api.impl

import api.EffectApi
import me.katze.gui4s.widget

trait EffectApiImpl[
  F[+_], 
  Update[+_, +_],
  Draw,
  Recompose,
  Place[+_],
  WidgetTaskIn[+_],
  SystemEvent
] extends EffectApi:
  override type WidgetTask[T] = WidgetTaskIn[T]
  override type Widget[+T] = Place[widget.Widget[Update, Draw, Place, Recompose, T, SystemEvent]]

  override def sideEffect[T](name : String, task: WidgetTask[T]): Widget[T] =
    ???
  end sideEffect

  override def launchedEffect[T](name : String, keys: Any*)(task: WidgetTask[T]): Widget[T] =
    ???
  end launchedEffect
end EffectApiImpl
