package me.katze.gui4s.widget
package stateful

import scala.annotation.tailrec

final case class EventReaction[+WidgetTask, +State, +ParentUpEvent](
                                                                      newState   : State,
                                                                      parentEvent: List[ParentUpEvent] = Nil,
                                                                      ios        : List[(WidgetTask, Boolean)] = Nil
                                                                    ):
  def mapState[NewState](f : State => NewState) : EventReaction[WidgetTask, NewState, ParentUpEvent] =
    EventReaction(f(newState), parentEvent, ios)
  end mapState
end EventReaction

given[Task]: BiMonad[[A, B] =>> EventReaction[Task, A, B]] with
  override def flatMap_[A, B, C](value: EventReaction[Task, A, B])
                                (f: A => EventReaction[Task, C, B]): EventReaction[Task, C, B] =
    val newValue = f(value.newState)
    EventReaction(
      newValue.newState,
      value.parentEvent ++ newValue.parentEvent,
      value.ios ++ newValue.ios
    )
  end flatMap_

  extension [A](value: A)
    override def asMonad: EventReaction[Task, A, Nothing] = EventReaction(value, Nil, Nil)

  override def tailRecM[A, B, E](a: A)
                                (f: A => EventReaction[Task, Either[A, B], E]): EventReaction[Task, B, E] =
    @tailrec
    def helper(
                a: A,
                parentEvent: List[E] = Nil,
                ios        : List[(Task, Boolean)] = Nil
              )(
                f: A => EventReaction[Task, Either[A, B], E]
              ) : EventReaction[Task, B, E] =
      val EventReaction(aa, ape, aios) = f(a)
      aa match
        case Left(value) => helper(value, ape ++ parentEvent, ios ++ aios)(f)
        case Right(value) => EventReaction(value, ape ++ parentEvent, ios ++ aios)
      end match
    end helper
    helper(a)(f)
  end tailRecM
end given

given[Task] : CatchEvents[[A, B] =>> EventReaction[Task, A, B]] with
  extension [W, E](old: EventReaction[Task, W, E]) 
    override def catchEvents: EventReaction[Task, (W, List[E]), Nothing] = 
      EventReaction[Task, (W, List[E]), Nothing]((old.newState, old.parentEvent), Nil, old.ios)
    end catchEvents
  end extension
end given  
