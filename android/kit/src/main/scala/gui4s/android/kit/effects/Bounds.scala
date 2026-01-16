package gui4s.android.kit.effects

import gui4s.core.geometry.{InfinityOr, Rect}

type Bounds = Rect[InfinityOr[Float]]

object Bounds:
  def apply(width : InfinityOr[Float], height : InfinityOr[Float]): Bounds =
    Rect(width, height)
  end apply

  def apply(width: Float, height: Float): Bounds = Rect(new InfinityOr(width), new InfinityOr(height))
end Bounds
