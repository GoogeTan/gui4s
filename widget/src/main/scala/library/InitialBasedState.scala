package me.katze.gui4s.widget
package library

import stateful.*

import cats.syntax.all.*

final case class InitialBasedState[
  Widget,
  Update[+_, +_],
  Place[+_],
  Dealloc,
  T: Equiv,
  ParentEvent, ChildEvent,
  WidgetTask[+_]
](
    initialState: T,
    currentState: T,
    eventHandler  : (T, ChildEvent) => EventReaction[WidgetTask[ChildEvent], T, ParentEvent],
    renderState   : T => Place[Widget],
    typeCheck     : RichTypeChecker[(T, T)],
    deallocState : T => Dealloc,
    liftEventReaction : LiftEventReaction[Update, WidgetTask[Any]]
) extends State[[U] =>> Update[U, ParentEvent], Dealloc, ChildEvent, Place[Widget]]:
  override def handleEvent(event: ChildEvent): Update[State[[U] =>> Update[U, ParentEvent], Dealloc, ChildEvent, Place[Widget]], ParentEvent] =
    liftEventReaction.lift(
      eventHandler(currentState, event).mapState(newState => copy(currentState = newState))
    )
  end handleEvent

  override def dealloc: Dealloc = deallocState(this.currentState)

  override def render: Place[Widget] = renderState(currentState)

  override def state: Any = (initialState, currentState)

  override def mergeWithOldState(maybeOldState: Any): State[[U] =>> Update[U, ParentEvent], Dealloc, ChildEvent, Place[Widget]] =
    val (oldInitialState, oldState) = typeCheck.tryCast(maybeOldState, "State type mismatch")
    if Equiv[T].equiv(oldInitialState, initialState) then
      copy(oldInitialState, currentState) // TODO Надо проверить, точно ли куррент. Кажется, будто надо брать старое, если оное есть.
    else
      this
    end if
  end mergeWithOldState
end InitialBasedState