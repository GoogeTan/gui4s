package me.katze.gui4s.widget
package stateful

trait RaiseEvent[+Update[+_, +_]]:
  def raise[Event](event : Event) : Update[Unit, Event]
end RaiseEvent
