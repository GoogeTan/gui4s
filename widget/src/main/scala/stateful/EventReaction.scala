package me.katze.gui4s.widget
package stateful

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
  override def flatMap_[A, B, C](value: EventReaction[A, B, Task])
                                (f: A => EventReaction[C, B, Task]): EventReaction[C, B, Task] =
    val newValue = f(value.newState)
    EventReaction(
      newValue.newState,
      value.parentEvent ++ newValue.parentEvent,
      value.ios ++ newValue.ios
    )
  end flatMap_

  extension [A](value: A)
    override def asMonad: EventReaction[A, Nothing, Task] = EventReaction(value, Nil, Nil)

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
  extension [W, E](old: EventReaction[W, E, Task])
    override def catchEvents: EventReaction[(W, List[E]), Nothing, Task] =
      EventReaction[(W, List[E]), Nothing, Task]((old.newState, old.parentEvent), Nil, old.ios)
    end catchEvents
  end extension
end given  
