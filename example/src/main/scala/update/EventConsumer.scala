package me.katze.gui4s.example
package update

@FunctionalInterface
trait EventConsumer[+FreeWidget, +F[+_], +UpEvent, -DownEvent]:
  def processEvent(event : DownEvent) : F[EventProcessResult[FreeWidget, UpEvent]]
end EventConsumer
