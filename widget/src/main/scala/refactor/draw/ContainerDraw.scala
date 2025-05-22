package me.katze.gui4s.widget
package refactor.draw

import refactor.Container

import cats.Monoid
import cats.syntax.all.*

final class ContainerDraw[Widget, Layout, Draw : Monoid](drawWidget : Drawable[Widget, Draw]) extends Drawable[Container[Widget, Layout], Draw]:
  override def draw(self: Container[Widget, Layout]): Draw =
    self.children.foldMap(drawWidget.draw)
  end draw
end ContainerDraw
  
