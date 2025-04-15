package me.katze.gui4s.widget
package stateful

trait RaiseEvent[+Update]:
  def raise(event : Any) : Update
end RaiseEvent
