package me.katze.gui4s.example
package update

final case class EventProcessResult[+Widget, +Event](widget: Widget, events: List[Event])