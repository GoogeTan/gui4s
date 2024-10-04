package me.katze.gui4s.example
package task

import me.katze.gui4s.widget.stateful.Path

final case class PathMapImpl[T](realMap : Map[Path, T]) extends PathMap[T]:
  override def add(key  : Path, value: T): PathMap[T] =
    PathMapImpl(realMap.+((key, value)))
  end add

  override def keys: Set[Path] = realMap.keySet

  override def remove(key: Path): (PathMap[T], Option[T]) =
    val value = realMap.get(key)
    (PathMapImpl(realMap.-(key)), value)
  end remove
end PathMapImpl

