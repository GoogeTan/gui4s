package me.katze.gui4s.example
package update

@FunctionalInterface
trait EventConsumer[+T, +F[+_], -DownEvent, +UpEvent]:
  def processEvent(event : DownEvent) : F[EventProcessResult[T, UpEvent]]
end EventConsumer
