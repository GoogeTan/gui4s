package me.katze.gui4s.example
package api

import me.katze.gui4s.widget.stateful.{EventReaction, RichTypeChecker}

trait StatefulApi extends HighLevelApi:
  def stateful[T: Equiv, ParentEvent, ChildEvent](
                                                    name        : String,
                                                    initialState: T,
                                                    eventHandler: (T, ChildEvent) => EventReaction[WidgetTask[ChildEvent], T, ParentEvent],
                                                  )(
                                                    renderState: T => Widget[ChildEvent]
                                                  )(
                                                    using RichTypeChecker[ChildEvent], RichTypeChecker[(T, T)]
                                                  ): Widget[ParentEvent]
end StatefulApi
