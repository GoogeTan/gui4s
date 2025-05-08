package me.katze.gui4s.example
package update

trait MultiMap[K, V]:
  def add(key : K, value : V) : MultiMap[K, V]
  def removeAll(key: K) : (MultiMap[K, V], Set[V])
  def remove(key: K, value : V) : MultiMap[K, V]
  def keys : Set[K]
  def values(key : K) : Set[V]
end MultiMap

