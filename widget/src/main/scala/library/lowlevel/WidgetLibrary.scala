package me.katze.gui4s.widget
package library.lowlevel

import stateful.Mergeable

import cats.*
import me.katze.gui4s
import me.katze.gui4s.widget

trait WidgetLibrary:
  type Draw
  type PlacedWidget[+A, -B]
  type WidgetTask[+T]
  type SystemEvent
  type PlacementEffect[+W]
  final type FreeWidget[+A, -B] = PlacementEffect[PlacedWidget[A, B]]
  final type Widget[+A] = FreeWidget[A, SystemEvent]
  
  given placementIsEffect: Functor[PlacementEffect]
  given freeTreesAreMergeable[A, B]: Mergeable[FreeWidget[A, B]]
  
  def constructRealWidget[RaisableEvent, HandleableEvent](widget: me.katze.gui4s.widget.PlacedWidget[Draw, WidgetTask[Any], FreeWidget, RaisableEvent, HandleableEvent]): PlacedWidget[RaisableEvent, HandleableEvent]
end WidgetLibrary

