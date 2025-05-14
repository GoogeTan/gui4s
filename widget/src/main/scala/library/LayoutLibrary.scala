package me.katze.gui4s.widget
package library

import cats.*
import cats.syntax.all.given
import me.katze.gui4s.widget
import me.katze.gui4s.widget.layout.{LayoutDraw, LayoutWidget}

type LayoutPlacementStrategy[Widget, PlacedWidget, PlacementEffect[+_], ChildrenMeta] =  List[Widget] => PlacementEffect[List[(PlacedWidget, ChildrenMeta)]]

def layoutWidget[
  Update[+_] : Monad,
  Draw,
  Place[+_] : FlatMap,
  Recomposition : Monoid,
  ChildrenMeta,
  DownEvent,
](
  using LayoutDraw[Draw, ChildrenMeta]
)(
  children         : List[Place[Widget[Update, Draw, Place, Recomposition, DownEvent]]],
  placementStrategy: LayoutPlacementStrategy[Place[Widget[Update, Draw, Place, Recomposition, DownEvent]], Widget[Update, Draw, Place, Recomposition, DownEvent], Place, ChildrenMeta]
): Place[Widget[Update, Draw, Place, Recomposition, DownEvent]] =
  placementStrategy(children).map(LayoutWidget(_, layoutWidget(_, placementStrategy)))
end layoutWidget