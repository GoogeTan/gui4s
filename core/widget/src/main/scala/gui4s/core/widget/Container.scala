package gui4s.core.widget

final case class Container[Widgets, Layout](children : Widgets, layout : Layout)