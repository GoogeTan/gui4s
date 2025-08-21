package me.katze.gui4s.widget.library

import catnip.BiMonad
import catnip.syntax.all.{*, given}
import cats.data.NonEmptyList
import cats.syntax.all.*
import me.katze.gui4s.widget.CatchEvents
import me.katze.gui4s.widget.handle.HandlesEventF

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
  Update[Event, Value] : BiMonad,
  Destructor[_]
](
  statefulWidget: StatefulWidget[Widget, Update, Destructor],
  raiseEvents : [Event] => List[Event] => Update[Event, Unit],
  //catchEvents : [E1, E2, V] => Update[E1, V] => Update[E2, (List[E1], E2)]
) extends TransitiveStatefulWidget[Widget, Update]:
  override def apply[State: Typeable, TransitiveEvent, OwnEvent](
                                                                  name: String,
                                                                  initialState: State,
                                                                  eventHandler: HandlesEventF[State, NonEmptyList[OwnEvent], Update[TransitiveEvent, *]],
                                                                  body: State => Widget[Either[OwnEvent, TransitiveEvent]]
                                                                ): Widget[TransitiveEvent] =
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
 
  
