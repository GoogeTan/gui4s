package me.katze.gui4s.example
package task

final case class IOOnThread[+Fiber](keepAfterWidgetDeath: Boolean, fiberControl: Fiber)
