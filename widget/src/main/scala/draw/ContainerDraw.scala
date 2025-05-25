package me.katze.gui4s.widget
package draw

import cats.Monoid
import cats.syntax.all.*

def drawContainer[Widget, Layout, Draw : Monoid](drawWidget : Drawable[Widget, Draw]) : Drawable[Container[Widget, Layout], Draw] =
  _.children.foldMap(drawWidget)
end drawContainer
