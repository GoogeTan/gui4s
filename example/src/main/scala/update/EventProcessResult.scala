package me.katze.gui4s.example
package update

final case class EventProcessResult[+FreeWidget, +Event](freeWidget : FreeWidget, events : List[Event])