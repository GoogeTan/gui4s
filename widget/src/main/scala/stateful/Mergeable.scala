package me.katze.gui4s.widget
package stateful

trait Mergeable[+Update[_], T]:
  def merge(oldOne : T, newOne : T) : Update[T]
end Mergeable
