package gui4s.core.widget
package draw

import cats._
import cats.syntax.all._

def drawContainer[
  Widget : Order as WO, 
  Collection[_] : Foldable, 
  Layout, 
  Draw : Monoid
](drawWidget : Drawable[Widget, Draw]) : Drawable[Container[Collection[Widget], Layout], Draw] =
  given Ordering[Widget] = (a,b) => WO.compare(a, b)
  _.children.toList.sorted.foldMap(drawWidget)
end drawContainer
