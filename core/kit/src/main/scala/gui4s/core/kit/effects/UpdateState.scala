package gui4s.core.kit
package effects

import cats.Monoid

final case class UpdateState[Point, Clip](consumed : Boolean, widgetCoordinates : Point, path: Clip):
  def withCoordinates(point: Point): UpdateState[Point, Clip] =
    copy(widgetCoordinates = point)
  end withCoordinates

  def markEventHandled : UpdateState[Point, Clip] =
    copy(consumed = true)
  end markEventHandled

  def withClip(path : Clip) : UpdateState[Point, Clip] =
    copy(path = path)
  end withClip
end UpdateState

object UpdateState:
  def empty[Point : Monoid as N, Clip : Monoid as ClipM] : UpdateState[Point, Clip] =
    UpdateState(false, N.empty, ClipM.empty)
  end empty
end UpdateState
