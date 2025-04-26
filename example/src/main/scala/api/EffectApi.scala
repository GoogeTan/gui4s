package me.katze.gui4s.example
package api

type LaunchedEffectWidget[Widget[_], WidgetTask[_]] = [Event] => (String, List[Any]) => WidgetTask[Event] => Widget[Event]
type SideEffectWidget[Widget[_], WidgetTask[_]] = [Event] => String => WidgetTask[Event] => Widget[Event]

trait EffectApi[Widget[_], WidgetTask[_]]:
  def launchedEffect[T](name : String, keys : Any*)(task : WidgetTask[T]) : Widget[T]
  def sideEffect[T](name : String, task : WidgetTask[T]) : Widget[T]
end EffectApi
