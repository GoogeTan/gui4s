package me.katze.gui4s.example
package api

type LaunchedEffectWidget[Widget[_], WidgetTask[_]] = [Event] => (String, List[Any]) => WidgetTask[Event] => Widget[Event]
type SideEffectWidget[Widget[_], WidgetTask[_]] = [Event] => String => WidgetTask[Event] => Widget[Event]

trait EffectApi[Widget[_], WidgetTask[_]]:
  def launchedEffect[Event](name : String, keys : Any*)(task : WidgetTask[Event]) : Widget[Event]
  def sideEffect[Event](name : String, task : WidgetTask[Event]) : Widget[Event]
end EffectApi
