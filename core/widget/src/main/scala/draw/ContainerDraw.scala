package gui4s.core.widget
package draw

import cats.*
import cats.syntax.all.*

def drawContainer[Widget : Order as WO, C[_] : Foldable, Layout, Draw : Monoid](drawWidget : Drawable[Widget, Draw]) : Drawable[Container[C[Widget], Layout], Draw] =
  given Ordering[Widget] = WO.compare
  _.children.toList.sorted.foldMap(drawWidget)
end drawContainer
