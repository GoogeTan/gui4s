package gui4s.core.widget.library.decorator

import catnip.syntax.all._
import cats._

import gui4s.core.widget.library.ContainerWidget
import gui4s.core.widget.library.decorator.Decorator

def minSizeWidget[
  Widget,
  OuterPlace[_] : FlatMap as M,
  Situated[_],
  Bounds,
  Point
](
   containerWidget: ContainerWidget[Widget, Id, OuterPlace * Situated, Point],
   ensureMinimalSize : (Situated[Widget], Bounds) => OuterPlace[Situated[(Widget, Point)]]
)(minSize : Bounds) : Decorator[OuterPlace[Situated[Widget]]] =
  containerWidget(
    _,
    M.flatMap(_)(ensureMinimalSize(_, minSize))
  )
end minSizeWidget
