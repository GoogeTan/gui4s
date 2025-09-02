package gui4s.desktop.widget.library

import catnip.BiMonad
import cats.Monad
import cats.data.NonEmptyList
import cats.syntax.all.*
import gui4s.core.widget.handle.HandlesEventF

import scala.reflect.Typeable

trait TransitiveStatefulWidget[Widget[_], Update[Event, Value]]:
  def apply[State: Typeable, TransitiveEvent, OwnEvent](
                                                          name: String,
                                                          initialState: State,
                                                          eventHandler: HandlesEventF[State, NonEmptyList[OwnEvent], Update[TransitiveEvent, *]],
                                                          body: State => Widget[Either[OwnEvent, TransitiveEvent]],
                                                        ): Widget[TransitiveEvent]

final class TransitiveStatefulWidgetFromStatefulWidget[
  Widget[_],
  Update[Event, Value] : BiMonad as updateBiMonad,
  Destructor[_]
](
  statefulWidget: StatefulWidget[Widget, Update, Destructor],
  raiseEvents : [Event] => List[Event] => Update[Event, Unit],
) extends TransitiveStatefulWidget[Widget, Update]:
  override def apply[State: Typeable, TransitiveEvent, OwnEvent](
                                                                  name: String,
                                                                  initialState: State,
                                                                  eventHandler: HandlesEventF[State, NonEmptyList[OwnEvent], Update[TransitiveEvent, *]],
                                                                  body: State => Widget[Either[OwnEvent, TransitiveEvent]]
                                                                ): Widget[TransitiveEvent] =
    given Monad[Update[TransitiveEvent, *]] = updateBiMonad()
    statefulWidget[State, TransitiveEvent, Either[OwnEvent, TransitiveEvent]](
      name,
      initialState,
      (state, path, events) => {
        val (ownEvents, transitiveEvents) = events.toList.partitionMap(identity)
        raiseEvents(transitiveEvents)
          *> NonEmptyList
              .fromList(ownEvents)
              .map(eventHandler(state, path, _))
              .getOrElse(state.pure)
      },
      body
    )
  end apply
end TransitiveStatefulWidgetFromStatefulWidget
