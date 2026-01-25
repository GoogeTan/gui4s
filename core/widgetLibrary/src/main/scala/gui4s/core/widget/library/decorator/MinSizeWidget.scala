package gui4s.core.widget.library.decorator

import catnip.syntax.all._
import cats._

import gui4s.core.widget.library.ContainerWidget
import gui4s.core.widget.library.decorator.Decorator

def minSizeWidget[
  Widget,
  PlacementEffect[_] : FlatMap as M,
  Situated[_],
  Bounds,
  Point
](
   containerWidget: ContainerWidget[Widget, Id, PlacementEffect * Situated, Point],
   ensureMinimalSize : (Situated[Widget], Bounds) => PlacementEffect[Situated[(Widget, Point)]]
)(minSize : Bounds) : Decorator[PlacementEffect[Situated[Widget]]] =
  containerWidget(
    _,
    M.flatMap(_)(ensureMinimalSize(_, minSize))
  )
end minSizeWidget
