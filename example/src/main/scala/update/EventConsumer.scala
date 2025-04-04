package me.katze.gui4s.example
package update

import me.katze.gui4s.widget.EventResult

@FunctionalInterface
trait EventConsumer[Update[+_, +_], +FreeWidget, +UpEvent, -DownEvent]:
  def processEvent(event : DownEvent) : Update[FreeWidget, UpEvent]
end EventConsumer
