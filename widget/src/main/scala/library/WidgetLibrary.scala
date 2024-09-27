package me.katze.gui4s.widget
package library

import stateful.{Mergeable, TaskFinished}

import cats.*
import me.katze.gui4s
import me.katze.gui4s.widget

trait WidgetLibrary:
  type Draw
  type PlacedWidget[+A, -B]
  type WidgetTask[+T]
  type SystemEvent >: TaskFinished
  type PlacementEffect[+W]
  final type FreeWidget[+A, -B] = PlacementEffect[PlacedWidget[A, B]]
  final type Widget[+A] = FreeWidget[A, SystemEvent]
  
  given placementIsEffect: Monad[PlacementEffect]
  given freeTreesAreMergeable[A, B]: Mergeable[FreeWidget[A, B]]
  
  def constructRealWidget[RaisableEvent, HandleableEvent](widget: me.katze.gui4s.widget.PlacedWidget[Draw, WidgetTask[Any], FreeWidget, RaisableEvent, HandleableEvent]): PlacedWidget[RaisableEvent, HandleableEvent]
end WidgetLibrary

