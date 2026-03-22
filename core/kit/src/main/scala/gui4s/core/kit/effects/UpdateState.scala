package gui4s.core.kit
package effects

import cats.Monoid

final case class UpdateState[EnvironmentalEvents, Point, Clip](
  environmentalEvents : EnvironmentalEvents,
  widgetCoordinates : Point, 
  clip: Clip
):
  def withCoordinates(point: Point): UpdateState[EnvironmentalEvents, Point, Clip] =
    copy(widgetCoordinates = point)
  end withCoordinates

  def withClip(path : Clip) : UpdateState[EnvironmentalEvents, Point, Clip] =
    copy(clip = path)
  end withClip
  
  def withEnvironmentalEvents(events : EnvironmentalEvents) : UpdateState[EnvironmentalEvents, Point, Clip] =
    copy(environmentalEvents = events)
  end withEnvironmentalEvents
end UpdateState

object UpdateState:
  def empty[EnvironmentalEvents : Monoid as M, Point : Monoid as N, Clip : Monoid as ClipM] : UpdateState[EnvironmentalEvents, Point, Clip] =
    UpdateState(M.empty, N.empty, ClipM.empty)
  end empty
end UpdateState
