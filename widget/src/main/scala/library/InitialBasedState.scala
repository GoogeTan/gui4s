package me.katze.gui4s.widget
package library

import stateful.*

import cats.syntax.all.*

final case class InitialBasedState[
  Widget,
  Update[+_, +_] : LiftEventReaction as liftEventReaction,
  Place[+_],
  Dealloc,
  T: Equiv,
  ParentEvent, ChildEvent,
  WidgetTask[+_]
](
    initialState: T,
    currentState: T,
    eventHandler  : (T, ChildEvent) => EventReaction[T, ParentEvent, WidgetTask[ChildEvent]],
    renderState   : T => Place[Widget],
    typeCheck     : RichTypeChecker[(T, T)],
    deallocState : T => Dealloc,
) extends State[[U] =>> Update[U, ParentEvent], Dealloc, ChildEvent, Place[Widget], WidgetTask[ChildEvent]]:
  override def handleEvent(event: ChildEvent): (Update[State[[U] =>> Update[U, ParentEvent], Dealloc, ChildEvent, Place[Widget], WidgetTask[ChildEvent]], ParentEvent], List[WidgetTask[ChildEvent]]) =
    val EventReaction(a, b, c) = eventHandler(currentState, event).mapState(newState => copy(currentState = newState))
    (
      liftEventReaction.lift(EventReaction(a, b, Nil)),
      c
      )
  end handleEvent

  override def dealloc: Dealloc = deallocState(this.currentState)

  override def render: Place[Widget] = renderState(currentState)

  override def state: Any = (initialState, currentState)

  override def mergeWithOldState(maybeOldState: Any): State[[U] =>> Update[U, ParentEvent], Dealloc, ChildEvent, Place[Widget], WidgetTask[ChildEvent]] =
    val (oldInitialState, oldState) = typeCheck.tryCast(maybeOldState).getOrElse(panic("State type mismatch"))
    if Equiv[T].equiv(oldInitialState, initialState) then
      copy(oldInitialState, currentState) // TODO Надо проверить, точно ли куррент. Кажется, будто надо брать старое, если оное есть.
    else
      this
    end if
  end mergeWithOldState
end InitialBasedState