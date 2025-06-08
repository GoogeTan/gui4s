package me.katze.gui4s.example

import catnip.BiMonad
import cats.{Bimonad, Monad}
import cats.data.StateT
import me.katze.gui4s.widget.{CatchEvents, given}
import cats.syntax.all.*
import scala.annotation.tailrec
import cats.data.IndexedStateT.catsDataMonadForIndexedStateT

final case class EventResult_[+Event, +Widget](
                                                widget: Widget,
                                                events: List[Event],
                                              ):
  def this(widget : Widget) =
    this(widget, Nil)
  end this
end EventResult_

type EventResult[Event, Widget] = StateT[EventResult_[Event, *], Boolean, Widget]

given BiMonad[EventResult] = [Event] => () => catsDataMonadForIndexedStateT(using eventResultIsBimonad)
given eventResultIsBiMonad : BiMonad[EventResult_] = [Event] => () => eventResultIsBimonad
given CatchEvents[EventResult] = liftStateTCatchEvents(using eventResultIsBiMonad, eventResult_CatchEvents)

given eventResultIsBimonad[Event] : Bimonad[[Value] =>> EventResult_[Event, Value]] with
  def pure[A](a: A): EventResult_[Event, A] = 
    EventResult_(a, Nil)
  end pure

  def flatMap[A, B](fa: EventResult_[Event, A])(f: A => EventResult_[Event, B]): EventResult_[Event, B] = 
    val result = f(fa.widget)
    EventResult_(result.widget, fa.events ++ result.events)
  end flatMap
    
  def map[A, B](fa: EventResult_[Event, A])(f: A => B): EventResult_[Event, B] = 
    EventResult_(f(fa.widget), fa.events)
  end map
    
  def coflatMap[A, B](fa: EventResult_[Event, A])(f: EventResult_[Event, A] => B): EventResult_[Event, B] = 
    EventResult_(f(fa), fa.events)
  end coflatMap
    
  def extract[A](fa: EventResult_[Event, A]): A = 
    fa.widget
  end extract
    
  def tailRecM[A, B](a: A)(f: A => EventResult_[Event, Either[A, B]]): EventResult_[Event, B] = 
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
    override def catchEvents[NewEventType]: EventResult_[Nothing, (Widget, List[Event])] =
      EventResult_((old.widget, old.events), Nil)
    end catchEvents
  end extension
end eventResult_CatchEvents
