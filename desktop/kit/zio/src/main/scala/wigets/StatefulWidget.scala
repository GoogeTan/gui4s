package gui4s.desktop.kit.zio
package widgets

import effects.*
import effects.Update.given
import effects.OuterPlace.given
import effects.Place.given
import effects.RecompositionReaction.given

import gui4s.core.widget.handle.HandlesEventF
import gui4s.decktop.widget.library.*
import gui4s.decktop.widget.library
import cats.data.*
import gui4s.core.widget.Path
import gui4s.core.layout.Sized.given
import gui4s.core.widget.StatefulState
import gui4s.core.widget.given
import catnip.syntax.all.given
import scala.reflect.Typeable

@SuppressWarnings(Array("org.wartremover.warts.All"))
def statefulWidget : StatefulWidget[DesktopWidget, Update, [State] =>> State => RecompositionReaction] =
  new StatefulWidget[DesktopWidget, Update,  [State] =>> State => RecompositionReaction]:
    override def apply[State: Typeable, Event, ChildEvent](
                                                            name: String,
                                                            initialState: State,
                                                            eventHandler: HandlesEventF[State, NonEmptyList[ChildEvent], UpdateC[Event]],
                                                            body: State => DesktopWidget[ChildEvent]
                                                          ): DesktopWidget[Event] =
      apply(name, initialState, eventHandler, body, _ => RecompositionReaction.empty)
    end apply

    @SuppressWarnings(Array("org.wartremover.warts.All"))
    override def apply[State: Typeable, Event, ChildEvent](
                                                            name: String,
                                                            initialState: State,
                                                            eventHandler: HandlesEventF[State, NonEmptyList[ChildEvent], UpdateC[Event]],
                                                            body: State => DesktopWidget[ChildEvent],
                                                            destructor: State => RecompositionReaction
                                                          ): DesktopWidget[Event] =
      library.stateful[
        UpdateC[Event],
        UpdateC[ChildEvent],
        Place,
        Draw,
        RecompositionReaction,
        DownEvent,
        State,
        ChildEvent
      ](
        widgetsAreMergeable = widgetsAreMergable[UpdateC[ChildEvent], OuterPlace, InnerPlace, Draw, RecompositionReaction, DownEvent],
        typeCheckState = Place.typecheckS[StatefulState[State]]((value : Any, path : Path) => "Error in stateful typechecking at " + path.toString + " with value [" + value.toString + "]"),
        liftUpdate = Update.catchEvents[List[ChildEvent], List[Event]]
      )(
        name = name,
        initialState = initialState,
        handleEvent = eventHandler,
        render = body,
        destructor = destructor
      )
    end apply
  end new
end statefulWidget
