package me.katze.gui4s.example
package api

trait MapWidgetLibrary[Widget[_]]:
  extension[A] (widget : Widget[A])
    def mapEvent[B](f : A => B) : Widget[B]
end MapWidgetLibrary
