package me.katze.gui4s.widget

import stateful.{BiMonad, CatchEvents, RaiseEvent, RichTypeChecker, panic}

import scala.annotation.tailrec

final case class EventResult[+FreeWidget, +UpEvent](
                                                      widget: FreeWidget,
                                                      events: List[UpEvent],
                                                    )


given BiMonad[EventResult] with
  override def flatMap_[A, B, C](value: EventResult[A, B])
                                (f: A => EventResult[C, B]): EventResult[C, B] =
    val newValue = f(value.widget)
    EventResult(
      newValue.widget,
      value.events ++ newValue.events,
    )
  end flatMap_

  extension [A](value: A)
    override def asMonad: EventResult[A, Nothing] = EventResult(value, Nil)

  override def tailRecM[A, B, E](a: A)
                                (f: A => EventResult[Either[A, B], E]): EventResult[B, E] =
    @tailrec
    def helper(
                a: A,
                parentEvent: List[E],
              )(
                f: A => EventResult[Either[A, B], E]
              ) : EventResult[B, E] =
      val EventResult(aa, ape) = f(a)
      aa match
        case Left(value) => helper(value, ape ++ parentEvent)(f)
        case Right(value) => EventResult(value, ape ++ parentEvent)
      end match
    end helper
    helper(a, Nil)(f)
  end tailRecM
end given

given[Event : RichTypeChecker as RTC, Task] : RaiseEvent[EventResult[Unit, Event]] with
  override def raise(event: Any): EventResult[Unit, Event] =
    EventResult((), List(RTC.tryCast(event).getOrElse(panic("Event type mismatch"))))
  end raise
end given

given[Task] : CatchEvents[[A, B] =>> EventResult[A, B]] with
  extension [W, E](old: EventResult[W, E])
    override def catchEvents: EventResult[(W, List[E]), Nothing] =
      EventResult[(W, List[E]), Nothing]((old.widget, old.events), Nil)
    end catchEvents
  end extension
end given  
