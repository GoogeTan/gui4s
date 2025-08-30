package gui4s.desktop.kit
package widgets

import effects.*
import effects.Update.given
import effects.Place.given

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

def statefulWidget[IO[_]] : StatefulWidget[DesktopWidget[IO, *], Update[IO, *, *], [State] =>> State => RecompositionReaction[IO]] =
  new StatefulWidget[DesktopWidget[IO, *], Update[IO, *, *],  [State] =>> State => RecompositionReaction[IO]]:
    override def apply[State: Typeable, Event, ChildEvent](
                                                            name: String,
                                                            initialState: State,
                                                            eventHandler: HandlesEventF[State, NonEmptyList[ChildEvent], UpdateC[Event]],
                                                            body: State => DesktopWidget[IO, ChildEvent]
                                                          ): DesktopWidget[IO, Event] =
      apply(name, initialState, eventHandler, body, _ => RecompositionReaction.empty)
    end apply

    override def apply[State: Typeable, Event, ChildEvent](
                                                            name: String,
                                                            initialState: State,
                                                            eventHandler: HandlesEventF[State, NonEmptyList[ChildEvent], UpdateC[Event]],
                                                            body: State => DesktopWidget[IO, ChildEvent],
                                                            destructor: State => RecompositionReaction[IO]
                                                          ): DesktopWidget[IO, Event] =
      library.stateful[
        UpdateC[IO, Event],
        UpdateC[IO, ChildEvent],
        PlaceC[IO],
        Draw,
        RecompositionReaction[IO],
        DownEvent,
        State,
        ChildEvent
      ](
        widgetsAreMergeable = widgetsAreMergable[UpdateC[IO, ChildEvent], OuterPlace[IO, *], InnerPlace, Draw[IO], RecompositionReaction[IO], DownEvent],
        typeCheckState = Place.typecheck[StatefulState[State]]((value : Any, path : Path) => "Error in stateful typechecking at " + path.toString + " with value [" + value.toString + "]"),
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
