package me.katze.gui4s.example

import catnip.BiMonad
import me.katze.gui4s.widget.CatchEvents

import scala.annotation.tailrec

final case class EventResult[+FreeWidget, +UpEvent](
                                                      widget: FreeWidget,
                                                      events: List[UpEvent],
                                                    )


given BiMonad[EventResult] with
  override def flatMapFirst[A, B, C](value: EventResult[A, B])
                                    (f: A => EventResult[C, B]): EventResult[C, B] =
    val newValue = f(value.widget)
    EventResult(
      newValue.widget,
      value.events ++ newValue.events,
    )
  end flatMapFirst


  override def mapSecond[A, B, D](value : EventResult[A, B])(f : B => D) : EventResult[A, D] =
    EventResult(value.widget, value.events.map(f))
  end mapSecond

  override def pure[A](value: A): EventResult[A, Nothing] =
    EventResult(value, Nil)
  end pure

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

given[Task] : CatchEvents[EventResult] with
  extension [Widget, Event](old: EventResult[Widget, Event])
    override def catchEvents: EventResult[(Widget, List[Event]), Nothing] =
      EventResult[(Widget, List[Event]), Nothing]((old.widget, old.events), Nil)
    end catchEvents
  end extension
end given  
