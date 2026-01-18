package gui4s.desktop.kit
package widgets

import scala.reflect.Typeable

import catnip.syntax.all.given
import cats._
import cats.data._

import gui4s.core.layout.Sized.given
import gui4s.core.widget.Path
import gui4s.core.widget.StatefulState
import gui4s.core.widget.given
import gui4s.core.widget.handle.HandlesEventF
import gui4s.core.widget.library.StatefulWidget
import gui4s.core.widget.library._

import gui4s.desktop.kit.effects.Place.given
import gui4s.desktop.kit.effects._
import gui4s.desktop.widget.library
import gui4s.desktop.widget.library._


def statefulWidget[IO[_] : MonadThrow]
: StatefulWidget[
  DesktopWidget[IO, *],
  Update[IO, *, *],
  [State] =>> State => RecompositionReaction[IO],
  [State] =>> MergeStates[PlaceC[IO], State]
] =
  new StatefulWidget[
    DesktopWidget[IO, *],
    Update[IO, *, *],
    [State] =>> State => RecompositionReaction[IO],
    [State] =>> MergeStates[PlaceC[IO], State]
  ]:
    override def apply[State: {Equiv, Typeable}, Event, ChildEvent](
                                                                      name: String,
                                                                      initialState: State,
                                                                      eventHandler: HandlesEventF[State, NonEmptyList[ChildEvent], UpdateC[IO, Event]],
                                                                      body: State => DesktopWidget[IO, ChildEvent]
                                                                    ): DesktopWidget[IO, Event] =
      apply(name, initialState, eventHandler, body, _ => RecompositionReaction.empty)
    end apply

    @SuppressWarnings(Array("org.wartremover.warts.ToString"))
    override def apply[State: {Equiv as EQ, Typeable}, Event, ChildEvent](
                                                                      name: String,
                                                                      initialState: State,
                                                                      eventHandler: HandlesEventF[State, NonEmptyList[ChildEvent], UpdateC[IO, Event]],
                                                                      body: State => DesktopWidget[IO, ChildEvent],
                                                                      destructor: State => RecompositionReaction[IO]
                                                                    ): DesktopWidget[IO, Event] =
      apply(
        name = name,
        initialState = initialState,
        eventHandler = eventHandler,
        body = body,
        destructor = destructor,
        mergeStates = [T] => (oldState : StatefulState[State], newState : StatefulState[State], consumer : StatefulState[State] => Place[IO, T]) =>
          if EQ.equiv(oldState.initialState, newState.initialState) then
             consumer(oldState)  // TODO Это какое-то тонкое место, надо проверить, что оно работает как ожидатся
          else
             consumer(newState)
      )
    end apply

    override def apply[State, Event, ChildEvent](
                                                            name: String,
                                                            initialState: State,
                                                            eventHandler: HandlesEventF[State, NonEmptyList[ChildEvent], UpdateC[IO, Event]],
                                                            body: State => DesktopWidget[IO, ChildEvent],
                                                            destructor: State => RecompositionReaction[IO],
                                                            mergeStates: MergeStates[PlaceC[IO], State]
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
        typeCheckState = Place.typecheck[IO, StatefulState[State]]((value: Any, path: Path) => new Exception("Error in stateful typechecking at " + path.toString + " with value [" + value.toString + "]")),
        liftUpdate = Update.catchEvents[IO, ChildEvent, Event],
        addNameToPath = Place.addNameToPath,
      )(
        name = name,
        initialState = initialState,
        handleEvent = eventHandler,
        render = body,
        destructor = destructor,
        mergeStates = mergeStates
      )
  end new
end statefulWidget
