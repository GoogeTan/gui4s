package gui4s.core.widget
package free

import cats.Functor
import cats.syntax.all._

def widgetWithMetaAsFree[Place[_] : Functor, Widget, Meta](initial : AsFree[Widget, Place[Widget]]) : AsFree[(Widget, Meta), Place[(Widget, Meta)]] =
  (widget, meta) => initial(widget).map((_, meta))
end widgetWithMetaAsFree