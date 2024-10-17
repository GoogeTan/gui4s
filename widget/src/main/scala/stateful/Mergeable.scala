package me.katze.gui4s.widget
package stateful

trait Mergeable[T]:
  def merge(oldOne : T, newOne : T) : T
end Mergeable
