package me.katze.gui4s.example
package api.effects

import cats.Monoid

final case class UpdateEffectState[Point, Clip](consumed : Boolean, widgetCoordinates : Point, path: Clip):
  def withCoordinates(point: Point): UpdateEffectState[Point, Clip] =
    copy(widgetCoordinates = point)
  end withCoordinates

  def markEventHandled : UpdateEffectState[Point, Clip] =
    copy(consumed = true)
  end markEventHandled

  def withClip(path : Clip) : UpdateEffectState[Point, Clip] =
    copy(path = path)
  end withClip
end UpdateEffectState

object UpdateEffectState:
  def empty[Point : Monoid as N, Clip : Monoid as ClipM] : UpdateEffectState[Point, Clip] =
    UpdateEffectState(false, N.empty, ClipM.empty)
  end empty
end UpdateEffectState
