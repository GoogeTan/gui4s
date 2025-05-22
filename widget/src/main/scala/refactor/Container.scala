package me.katze.gui4s.widget
package refactor

final case class Container[Widget, Layout](children : List[Widget], layout : Layout)