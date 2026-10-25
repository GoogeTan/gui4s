package gui4s.core.kit
package effects

import cats.Monoid
import cats.kernel.Semigroup
import cats.syntax.all.*

import gui4s.core.widget.Path

final case class UpdateContext[Point, Clip](
  widgetCornerCoordinates : Point,
  path : Path,
  clip: Clip
):
  def withCoordinates(point: Point): UpdateContext[Point, Clip] =
    copy(widgetCornerCoordinates = point)
  end withCoordinates

  def withClip(path : Clip) : UpdateContext[Point, Clip] =
    copy(clip = path)
  end withClip
  
  def addNameToThePath(name: String) : UpdateContext[Point, Clip] =
    copy(path = path / name)
  end addNameToThePath  
end UpdateContext

object UpdateContext:
  def empty[Point : Monoid as N, Clip : Monoid as ClipM] : UpdateContext[Point, Clip] =
    UpdateContext(N.empty, Path(Nil), ClipM.empty)
  end empty
end UpdateContext


final case class UpdateState[EnvironmentalEvents](
  environmentalEvents : EnvironmentalEvents
):
  def withEnvironmentalEvents(events : EnvironmentalEvents) : UpdateState[EnvironmentalEvents] =
    copy(environmentalEvents = events)
  end withEnvironmentalEvents
  
  def emitEnvironmentalEvents(events : EnvironmentalEvents)(using Semigroup[EnvironmentalEvents]) : UpdateState[EnvironmentalEvents] =
    withEnvironmentalEvents(environmentalEvents |+| events)
  end emitEnvironmentalEvents
end UpdateState

object UpdateState:
  def empty[EnvironmentalEvents : Monoid as M] : UpdateState[EnvironmentalEvents] =
    UpdateState(M.empty)
  end empty
end UpdateState
