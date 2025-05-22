package me.katze.gui4s.widget

final case class Container[Widget, Layout](children : List[Widget], layout : Layout)