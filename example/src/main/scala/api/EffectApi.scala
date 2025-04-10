package me.katze.gui4s.example
package api

trait EffectApi[Widget[_], WidgetTask[_]]:
  def launchedEffect[T](name : String, keys : Any*)(task : WidgetTask[T]) : Widget[T]
  def sideEffect[T](name : String, task : WidgetTask[T]) : Widget[T]
end EffectApi
