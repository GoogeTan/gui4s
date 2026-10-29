package gui4s.desktop.kit
package effects

import cats.Eq
import gui4s.core.geometry.InfinityOr
import gui4s.core.geometry.Rect

type Bounds = Rect[InfinityOr[Float]]

object Bounds:
  def inf : Bounds =
    Rect(InfinityOr(None), InfinityOr(None))

  given Eq[Bounds] = Eq.by(bounds => (bounds.width, bounds.height))
end Bounds