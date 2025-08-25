package gui4s.core.widget
package draw

import cats.{Foldable, Monoid}
import cats.syntax.all.*

def drawContainer[Widget, C[_] : Foldable, Layout, Draw : Monoid](drawWidget : Drawable[Widget, Draw]) : Drawable[Container[C[Widget], Layout], Draw] =
  _.children.foldMap(drawWidget)
end drawContainer
