package me.katze.gui4s.widget
package library.lowlevel

import stateful.Mergeable

import cats.*
import me.katze.gui4s
import me.katze.gui4s.widget

type WidgetLibraryGeneric[
  UpdateIn[+_, +_],
  MergeIn[+_],
  PlaceIn[+_],
  DrawIn,
  PlacedWidgetIn[+_, -_],
  WidgetTaskIn[+_],
  SystemEventIn
] = WidgetLibrary
  {
    type PlacedWidget = PlacedWidgetIn
    type Update = UpdateIn
    type PlacementEffect = PlaceIn
    type Draw = DrawIn
    type Merge = MergeIn
    type SystemEvent = SystemEventIn
    type WidgetTask = WidgetTaskIn
  }

trait WidgetLibrary:
  type Draw
  type PlacedWidget[+A, -B]
  type WidgetTask[+T] // TODO remove generic
  type SystemEvent
  type PlacementEffect[+W]
  type Update[+_, +_]
  type Merge[+_]
  final type FreeWidget[+A, -B] = PlacementEffect[PlacedWidget[A, B]]
  final type Widget[+A] = FreeWidget[A, SystemEvent]
  
  given placementIsEffect: Functor[PlacementEffect]
  given freeTreesAreMergeable[A, B]: Mergeable[Merge, FreeWidget[A, B]]
  
  def constructRealWidget[RaisableEvent, HandleableEvent](widget: gui4s.widget.PlacedWidget[Update, Merge, Draw, WidgetTask[Any], FreeWidget, RaisableEvent, HandleableEvent]): PlacedWidget[RaisableEvent, HandleableEvent]
end WidgetLibrary

