package me.katze.gui4s.example
package task

final case class StlWrapperMultiMap[K, V](realMap: Map[K, Set[V]]) extends MultiMap[K, V]:
  override def add(key : K, value: V): MultiMap[K, V] =
    StlWrapperMultiMap(
      realMap
        .updated(
          key, 
          realMap.getOrElse(key, Set()).+(value)
        )
    )
  end add

  override def keys: Set[K] = realMap.keySet

  override def remove(key: K): (MultiMap[K, V], Set[V]) =
    val value = realMap.getOrElse(key, Set())
    (StlWrapperMultiMap(realMap.-(key)), value)
  end remove
end StlWrapperMultiMap

