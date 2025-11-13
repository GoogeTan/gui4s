package gui4s.desktop.kit
package widgets

import catnip.syntax.all.given
import cats.*
import cats.data.*
import gui4s.core.layout.Sized.given
import gui4s.core.widget.handle.HandlesEventF
import gui4s.core.widget.{Path, StatefulState, given}
import gui4s.desktop.kit.effects.*
import gui4s.desktop.kit.effects.Place.given
import gui4s.desktop.widget.library
import gui4s.desktop.widget.library.*

import scala.reflect.Typeable

def statefulWidget[IO[_] : MonadThrow] : StatefulWidget[DesktopWidget[IO, *], Update[IO, *, *], [State] =>> State => RecompositionReaction[IO]] =
  new StatefulWidget[DesktopWidget[IO, *], Update[IO, *, *],  [State] =>> State => RecompositionReaction[IO]]:
    override def apply[State: Typeable, Event, ChildEvent](
                                                            name: String,
                                                            initialState: State,
                                                            eventHandler: HandlesEventF[State, NonEmptyList[ChildEvent], UpdateC[IO, Event]],
                                                            body: State => DesktopWidget[IO, ChildEvent]
                                                          ): DesktopWidget[IO, Event] =
      apply(name, initialState, eventHandler, body, _ => RecompositionReaction.empty)
    end apply

    @SuppressWarnings(Array("org.wartremover.warts.ToString"))
    override def apply[State: Typeable, Event, ChildEvent](
                                                            name: String,
                                                            initialState: State,
                                                            eventHandler: HandlesEventF[State, NonEmptyList[ChildEvent], UpdateC[IO, Event]],
                                                            body: State => DesktopWidget[IO, ChildEvent],
                                                            destructor: State => RecompositionReaction[IO]
                                                          ): DesktopWidget[IO, Event] =
      library.stateful[
        UpdateC[IO, Event],
        UpdateC[IO, ChildEvent],
        PlaceC[IO],
        Draw[IO],
        RecompositionReaction[IO],
        DownEvent,
        State,
        ChildEvent
      ](
        widgetsAreMergeable = widgetsAreMergable[
          UpdateC[IO, ChildEvent],
          OuterPlace[IO, *],
          InnerPlace,
          Draw[IO],
          RecompositionReaction[IO],
          DownEvent
        ],
        typeCheckState = Place.typecheck[IO, StatefulState[State]]((value : Any, path : Path) => new Exception("Error in stateful typechecking at " + path.toString + " with value [" + value.toString + "]")),
        liftUpdate = Update.catchEvents[IO, ChildEvent, Event],
        addNameToPath = Place.addNameToPath
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
