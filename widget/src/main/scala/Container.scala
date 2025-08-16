package me.katze.gui4s.widget

final case class Container[Widgets, Layout](children : Widgets, layout : Layout)