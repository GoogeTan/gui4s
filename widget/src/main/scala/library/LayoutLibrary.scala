package me.katze.gui4s.widget
package library

import stateful.BiMonad

import cats.*
import cats.syntax.all.given
import me.katze.gui4s.widget

type LayoutPlacementStrategy[Widget, PlacedWidget, PlacementEffect[+_], ChildrenMeta] =  List[Widget] => PlacementEffect[List[(PlacedWidget, ChildrenMeta)]]

def layoutWidget[
  Update[+_, +_] : BiMonad,
  Draw,
  Place[+_] : FlatMap,
  Recomposition : Monoid,
  ChildrenMeta,
  Event,
  DownEvent,
](
  using LayoutDraw[Draw, ChildrenMeta]
)(
  children         : List[Place[Widget[Update, Draw, Place, Recomposition, Event, DownEvent]]],
  placementStrategy: LayoutPlacementStrategy[Place[Widget[Update, Draw, Place, Recomposition, Event, DownEvent]], Widget[Update, Draw, Place, Recomposition, Event, DownEvent], Place, ChildrenMeta]
): Place[Widget[Update, Draw, Place, Recomposition, Event, DownEvent]] =
  placementStrategy(children).map(LayoutWidget(_, layoutWidget(_, placementStrategy)))
end layoutWidget