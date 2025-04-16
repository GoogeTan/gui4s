package me.katze.gui4s.example
package api

trait MapWidgetLibrary[Widget[_]]:
  def mapEvent[A, B](widget: Widget[A])(f : A => B) : Widget[B]
end MapWidgetLibrary
