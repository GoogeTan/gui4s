package me.katze.gui4s.widget
package stateful

trait Mergeable[T]:
  def merge(oldOne : T, newOne : T) : T

  final def mergeThree(a : T, b : T, c : T) : T = merge(merge(a, b), c)
end Mergeable
