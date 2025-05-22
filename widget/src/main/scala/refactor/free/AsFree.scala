package me.katze.gui4s.widget
package refactor.free

trait AsFree[Placed, Free]:
  def asFree(self : Placed) : Free
