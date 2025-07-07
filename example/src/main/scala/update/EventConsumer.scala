package me.katze.gui4s.example
package update

@FunctionalInterface
trait EventConsumer[Update[_], FreeWidget, -DownEvent]:
  def processEvent(event : DownEvent) : Update[FreeWidget]
end EventConsumer
