package gui4s.core.widget.library.decorator

import catnip.syntax.all.{*, given}
import cats.*
import gui4s.core.widget.library.ContainerWidget
import gui4s.core.widget.library.decorator.Decorator

def minSizeWidget[
  Widget,
  OuterPlace[_] : FlatMap as M,
  InnerPlace[_],
  Bounds,
  Point
](
   containerWidget: ContainerWidget[Widget, Id, OuterPlace * InnerPlace, Point],
   ensureMinimalSize : (InnerPlace[Widget], Bounds) => OuterPlace[InnerPlace[(Widget, Point)]]
)(minSize : Bounds) : Decorator[OuterPlace[InnerPlace[Widget]]] =
  containerWidget(
    _,
    M.flatMap(_)(ensureMinimalSize(_, minSize))
  )
end minSizeWidget
