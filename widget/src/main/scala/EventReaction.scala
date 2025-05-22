package me.katze.gui4s.widget

import catnip.BiMonad

import scala.annotation.tailrec

final case class EventReaction[+State, +ParentUpEvent, +WidgetTask](
                                                                      newState   : State,
                                                                      parentEvent: List[ParentUpEvent],
                                                                      ios        : List[WidgetTask]
                                                                    ):
  def mapState[NewState](f : State => NewState) : EventReaction[NewState, ParentUpEvent, WidgetTask] =
    copy(newState = f(newState))
  end mapState

  def mapEvent[NewEvent](f : ParentUpEvent => NewEvent) : EventReaction[State, NewEvent, WidgetTask] =
    copy(parentEvent = parentEvent.map(f))
  end mapEvent

  def mapIOS[NewWidgetTask](f : WidgetTask => NewWidgetTask) : EventReaction[State, ParentUpEvent, NewWidgetTask] =
    copy(ios = ios.map(f))
  end mapIOS
end EventReaction

given[Task]: BiMonad[[A, B] =>> EventReaction[A, B, Task]] with
  override def flatMapFirst[A, B, C](value: EventReaction[A, B, Task])
                                    (f: A => EventReaction[C, B, Task]): EventReaction[C, B, Task] =
    val newValue = f(value.newState)
    EventReaction(
      newValue.newState,
      value.parentEvent ++ newValue.parentEvent,
      value.ios ++ newValue.ios
    )
  end flatMapFirst

  override def pure[A](value: A): EventReaction[A, Nothing, Task] =  
    EventReaction(value, Nil, Nil)
  
  override def mapSecond[A, B, D](value : EventReaction[A, B, Task])(f : B => D) : EventReaction[A, D, Task] =
    value.mapEvent(f)
  end mapSecond
  
  override def tailRecM[A, B, E](a: A)
                                (f: A => EventReaction[Either[A, B], E, Task]): EventReaction[B, E, Task] =
    @tailrec
    def helper(
                a: A,
                parentEvent: List[E],
                ios        : List[Task]
              )(
                f: A => EventReaction[Either[A, B], E, Task]
              ) : EventReaction[B, E, Task] =
      val EventReaction(aa, ape, aios) = f(a)
      aa match
        case Left(value) => helper(value, ape ++ parentEvent, ios ++ aios)(f)
        case Right(value) => EventReaction(value, ape ++ parentEvent, ios ++ aios)
      end match
    end helper
    helper(a, Nil, Nil)(f)
  end tailRecM
end given

given[Task] : CatchEvents[[A, B] =>> EventReaction[A, B, Task]] with
  extension [Widget, Event](old: EventReaction[Widget, Event, Task])
    override def catchEvents: EventReaction[(Widget, List[Event]), Nothing, Task] =
      EventReaction[(Widget, List[Event]), Nothing, Task]((old.newState, old.parentEvent), Nil, old.ios)
    end catchEvents
  end extension
end given  
