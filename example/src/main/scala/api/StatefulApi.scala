package me.katze.gui4s.example
package api

import me.katze.gui4s.widget.stateful.{EventReaction, RichTypeChecker}

trait StatefulApi[Widget[_], WidgetTask[_], Recomposition]:
  def stateful[State: {Equiv, RichTypeChecker}, ParentEvent, ChildEvent : RichTypeChecker](
                                                        name        : String,
                                                        initialState: State,
                                                        dealloc : State => Recomposition,
                                                        eventHandler: (State, ChildEvent) => EventReaction[State, ParentEvent, WidgetTask[ChildEvent]],
                                                      )(
                                                        renderState: State => Widget[ChildEvent]
                                                      ): Widget[ParentEvent]
  
  // TODO Надо подумать, может такое состояние должно стоять не над, а рядом с детскими состояниями, чтобы его можно было добавлять и убавлять.
  def stateInBetween[
    State : {Equiv, RichTypeChecker}, 
    TransitiveEvent : RichTypeChecker, 
    OwnEvent : RichTypeChecker
  ](
    name : String,
    initialState : State,
    dealloc : State => Recomposition,
    eventHandler: (State, OwnEvent) => EventReaction[State, TransitiveEvent, WidgetTask[OwnEvent]],
  )(
    renderState : State => Widget[Either[TransitiveEvent, OwnEvent]]
  ) : Widget[TransitiveEvent]
end StatefulApi
