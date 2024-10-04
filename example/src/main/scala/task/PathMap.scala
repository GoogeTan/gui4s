package me.katze.gui4s.example
package task

import me.katze.gui4s.widget.stateful.Path

trait PathMap[T]:
  def add(key : Path, value : T) : PathMap[T]
  def remove(key : Path) : (PathMap[T], Option[T])
  def keys : Set[Path]
end PathMap

