package me.katze.gui4s.example
package api.exported

import api.exported.given

import catnip.syntax.all.{*, given}
import cats.Monad
import cats.data.NonEmptyList
import cats.syntax.all.*
import me.katze.gui4s.widget.library.StatefulWidget
import me.katze.gui4s.widget.{EventReaction, Path}

import scala.language.experimental.namedTypeArguments
import scala.reflect.Typeable

def skijaStatefulWidget[F[_]: Monad, UpdateError, PlaceError, MeasurementUnit, DownEvent](
  typecheckError: (Any, Path) => PlaceError,
): StatefulWidget[
  SkijaWidget[F, MeasurementUnit, UpdateError, PlaceError, *, DownEvent],
  Path => F[Unit]
] =
  new StatefulWidget[
    SkijaWidget[F, MeasurementUnit, UpdateError, PlaceError, *, DownEvent],
    Path => F[Unit]
  ]:
    override def apply[State : Typeable, Event, ChildEvent](
                                                              name: String,
                                                              initialState: State,
                                                              eventHandler: (State, NonEmptyList[ChildEvent]) => EventReaction[State, Event, Path => F[Unit]],
                                                              body: State => SkijaWidget[F, MeasurementUnit, UpdateError, PlaceError, ChildEvent, DownEvent]
                                                            ): SkijaWidget[F, MeasurementUnit, UpdateError, PlaceError, Event, DownEvent] =

        skijaStateful(
          name,
          initialState,
          eventHandler,
          body,
          (_ : State) => ().pure[F],
          typecheckError
        )
    end apply
  end new
end skijaStatefulWidget