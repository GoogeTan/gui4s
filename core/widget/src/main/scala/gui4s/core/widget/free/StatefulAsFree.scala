package gui4s.core.widget
package free

import cats.Functor
import cats.syntax.functor._

def statefulAsFree[
  Place[_] : Functor,
  Widget,
  State,
](
  widgetAsFree : AsFree[Widget, Place[Widget]]
) : AsFree[Stateful[Widget, State], Place[Stateful[Widget, State]]] =
  self => widgetAsFree(self.child).map(child => self.copy(child = child))
end statefulAsFree