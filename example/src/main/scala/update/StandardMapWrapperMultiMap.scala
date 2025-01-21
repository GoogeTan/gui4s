package me.katze.gui4s.example
package update

final case class StandardMapWrapperMultiMap[K, V](realMap: Map[K, Set[V]]) extends MultiMap[K, V]:
  def this() =
    this(Map())
  end this
  
  override def add(key : K, value: V): MultiMap[K, V] =
    StandardMapWrapperMultiMap(
      realMap
        .updated(
          key, 
          realMap.getOrElse(key, Set()).+(value)
        )
    )
  end add

  override def keys: Set[K] = realMap.keySet

  override def values(key: K): Set[V] =
    realMap.getOrElse(key, Set())
  end values
  override def remove(key: K, value: V): MultiMap[K, V] =
    StandardMapWrapperMultiMap(
      realMap
        .updated(
          key,
          realMap.getOrElse(key, Set()).excl(value)
        )
    )
  end remove
  
  override def removeAll(key: K): (MultiMap[K, V], Set[V]) =
    val value = realMap.getOrElse(key, Set())
    (StandardMapWrapperMultiMap(realMap.-(key)), value)
  end removeAll
end StandardMapWrapperMultiMap

