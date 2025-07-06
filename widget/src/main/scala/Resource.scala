package me.katze.gui4s.widget

final case class Resource[Widget, Value](name : String, widget : Widget, value : Value)
