package me.katze.gui4s.example

import cats.data.{StateT, WriterT}
import me.katze.gui4s.layout.Point3d

final case class EventResultState[MeasurementUnit](consumed : Boolean, widgetCoordinates : Point3d[MeasurementUnit]):
  def addCoordinates(using Numeric[MeasurementUnit])(point : Point3d[MeasurementUnit]) : EventResultState[MeasurementUnit] =
    copy(widgetCoordinates = widgetCoordinates + point)
  end addCoordinates
end EventResultState

def emptyEventResultState[MeasurementUnit : Numeric as N] : EventResultState[MeasurementUnit] =
  EventResultState(false, Point3d(N.zero, N.zero, N.zero))
end emptyEventResultState

type EventResult[F[_], MeasurementUnit, Event, Widget] = StateT[WriterT[F, List[Event], *], EventResultState[MeasurementUnit], Widget]

def markEventHandled[MeasuremementUnit](state : EventResultState[MeasuremementUnit]) : EventResultState[MeasuremementUnit] =
  EventResultState(true, state.widgetCoordinates)
end markEventHandled