package me.katze.gui4s.widget
package free

import handle.Layout

import cats.Functor
import cats.syntax.functor.*

def containerAsFree[Place[_] : Functor, C[_] : Functor, Widget, Meta](
  widgetAsFree : AsFree[Widget, Place[Widget]]
) : AsFree[Container[C[(Widget, Meta)], Layout[Place, C, Widget, Meta]], Place[Container[C[(Widget, Meta)], Layout[Place, C, Widget, Meta]]]] =
  self =>
    val freeChildren = self.children.map((widget, _) => widgetAsFree(widget))
    self.layout(freeChildren).map(children => self.copy(children = children))
end containerAsFree
