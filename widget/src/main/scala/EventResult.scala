package me.katze.gui4s.widget

import stateful.{BiMonad, CatchEvents, RaiseEvent}

import scala.annotation.tailrec

type EventResultP[WT] = [A, B] =>> EventResult[WT, A, B]

final case class EventResult[+WidgetTask, +FreeWidget, +UpEvent](
                                                                  widget: FreeWidget, 
                                                                  upEvent: List[UpEvent],
                                                                  ios   : List[WidgetTask]
                                                                )


given[Task]: BiMonad[[A, B] =>> EventResult[Task, A, B]] with
  override def flatMap_[A, B, C](value: EventResult[Task, A, B])
                                (f: A => EventResult[Task, C, B]): EventResult[Task, C, B] =
    val newValue = f(value.widget)
    EventResult(
      newValue.widget,
      value.upEvent ++ newValue.upEvent,
      value.ios ++ newValue.ios
    )
  end flatMap_

  extension [A](value: A)
    override def asMonad: EventResult[Task, A, Nothing] = EventResult(value, Nil, Nil)

  override def tailRecM[A, B, E](a: A)
                                (f: A => EventResult[Task, Either[A, B], E]): EventResult[Task, B, E] =
    @tailrec
    def helper(
                a: A,
                parentEvent: List[E],
                ios        : List[Task]
              )(
                f: A => EventResult[Task, Either[A, B], E]
              ) : EventResult[Task, B, E] =
      val EventResult(aa, ape, aios) = f(a)
      aa match
        case Left(value) => helper(value, ape ++ parentEvent, ios ++ aios)(f)
        case Right(value) => EventResult(value, ape ++ parentEvent, ios ++ aios)
      end match
    end helper
    helper(a, Nil, Nil)(f)
  end tailRecM
end given

given[Task] : RaiseEvent[[A, B] =>> EventResult[Task, A, B]] with
  override def raise[Event](event: Event): EventResult[Task, Unit, Event] =
    EventResult((), List(event), Nil)
  end raise
end given  

given[Task] : CatchEvents[[A, B] =>> EventResult[Task, A, B]] with
  extension [W, E](old: EventResult[Task, W, E])
    override def catchEvents: EventResult[Task, (W, List[E]), Nothing] =
      EventResult[Task, (W, List[E]), Nothing]((old.widget, old.upEvent), Nil, old.ios)
    end catchEvents
  end extension
end given  
