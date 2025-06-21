package me.katze.gui4s.example

import catnip.BiMonad
import cats.Bimonad
import cats.data.IndexedStateT.catsDataMonadForIndexedStateT
import cats.data.StateT
import me.katze.gui4s.layout.Point3d
import me.katze.gui4s.widget.{CatchEvents, given}

import scala.annotation.tailrec

final case class EventResult_[+Event, +Widget](
                                                widget: Widget,
                                                events: List[Event],
                                              ):
  def this(widget : Widget) =
    this(widget, Nil)
  end this
end EventResult_

type EventResultState[MeasurementUnit] = (consumed : Boolean, widgetCoordinates : Point3d[MeasurementUnit])

def emptyEventResultState[MeasurementUnit : Numeric as N] : EventResultState[MeasurementUnit] =
  (false, Point3d(N.zero, N.zero, N.zero))
end emptyEventResultState

type EventResult[MeasurementUnit, Event, Widget] = StateT[EventResult_[Event, *], EventResultState[MeasurementUnit], Widget]

def markEventHandled[MeasuremementUnit](state : EventResultState[MeasuremementUnit]) : EventResultState[MeasuremementUnit] =
  (true, state.widgetCoordinates)
end markEventHandled

given[MeasurementUnit]: BiMonad[EventResult[MeasurementUnit, *, *]] = [Event] => () => catsDataMonadForIndexedStateT(using eventResultIsBimonad)
given eventResultIsBiMonad : BiMonad[EventResult_] = [Event] => () => eventResultIsBimonad
given[MeasurementUnit]: CatchEvents[EventResult[MeasurementUnit, *, *]] = liftStateTCatchEvents[EventResult_, EventResultState[MeasurementUnit]](using eventResultIsBiMonad, eventResult_CatchEvents)

given eventResultIsBimonad[Event] : Bimonad[[Value] =>> EventResult_[Event, Value]] with
  override def pure[A](a: A): EventResult_[Event, A] = 
    EventResult_(a, Nil)
  end pure

  override def flatMap[A, B](fa: EventResult_[Event, A])(f: A => EventResult_[Event, B]): EventResult_[Event, B] = 
    val result = f(fa.widget)
    EventResult_(result.widget, fa.events ++ result.events)
  end flatMap
    
  override def map[A, B](fa: EventResult_[Event, A])(f: A => B): EventResult_[Event, B] = 
    EventResult_(f(fa.widget), fa.events)
  end map
    
  override def coflatMap[A, B](fa: EventResult_[Event, A])(f: EventResult_[Event, A] => B): EventResult_[Event, B] = 
    EventResult_(f(fa), fa.events)
  end coflatMap
    
  override def extract[A](fa: EventResult_[Event, A]): A = 
    fa.widget
  end extract
    
  override def tailRecM[A, B](a: A)(f: A => EventResult_[Event, Either[A, B]]): EventResult_[Event, B] = 
    @tailrec
    def helper(current: EventResult_[Event, Either[A, B]], accEvents: List[Event]): EventResult_[Event, B] =
      current.widget match
        case Left(nextA) =>
          val next = f(nextA)
          helper(next, accEvents ++ current.events)
        case Right(b) =>
          EventResult_(b, accEvents ++ current.events)
      end match
    end helper
          
    helper(f(a), Nil)
  end tailRecM
end eventResultIsBimonad

given eventResult_CatchEvents: CatchEvents[EventResult_] with
  extension [Event, Widget](old: EventResult_[Event, Widget])
    override def catchEvents[NewEventType]: EventResult_[NewEventType, (List[Event], Widget)] =
      EventResult_((old.events, old.widget), Nil)
    end catchEvents
  end extension
end eventResult_CatchEvents
