package me.katze.gui4s.widget
package free

import handle.Layout

import cats.Functor
import cats.syntax.functor.*

def containerAsFree[Place[_] : Functor, Widget](
  widgetAsFree : AsFree[Widget, Place[Widget]]
) : AsFree[Container[Widget, Layout[Place, Widget]], Place[Container[Widget, Layout[Place, Widget]]]] =
  self => self.layout(self.children.map(widgetAsFree)).map(children => self.copy(children = children))
end containerAsFree
