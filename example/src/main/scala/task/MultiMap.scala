package me.katze.gui4s.example
package task

trait MultiMap[K, V]:
  def add(key : K, value : V) : MultiMap[K, V]
  def remove(key : K) : (MultiMap[K, V], Set[V])
  def keys : Set[K]
end MultiMap

