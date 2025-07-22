package me.katze.gui4s.example
package api

import api.exported.{*, given}

import catnip.ForeighFunctionInterface
import catnip.syntax.all.{*, given}
import cats.Monad
import cats.data.NonEmptyList
import cats.syntax.all.*
import io.github.humbleui.skija.shaper.Shaper
import me.katze.gui4s.layout.Sized
import me.katze.gui4s.skija.{Pixel, SkijaPlacedText, SkijaTextStyle}
import me.katze.gui4s.widget.library.StatefulWidget
import me.katze.gui4s.widget.{EventReaction, Path}
import scalacache.Cache

import scala.language.experimental.namedTypeArguments
import scala.reflect.Typeable

def makeSkijaStatefulWidget[F[_]: Monad, PlaceError, MeasurementUnit, DownEvent](
  typecheckError: (Any, Path) => PlaceError
): StatefulWidget[
  SkijaWidget[F, MeasurementUnit, PlaceError, *, DownEvent],
  Nothing
] =
  new StatefulWidget[
    SkijaWidget[F, MeasurementUnit, PlaceError, *, DownEvent],
    Nothing
  ]:
    override def apply[State : Typeable, Event, ChildEvent](
                                                              name: String,
                                                              initialState: State,
                                                              eventHandler: (State, NonEmptyList[ChildEvent]) => EventReaction[State, Event, Nothing],
                                                              body: State => SkijaWidget[F, MeasurementUnit, PlaceError, *, DownEvent][ChildEvent]
                                                            ): SkijaWidget[F, MeasurementUnit, PlaceError, *, DownEvent][Event] =

        skijaStateful(
          name,
          initialState,
          eventHandler,
          body,
          _ => ().pure[F],
          typecheckError
        )
    end apply
  end new
end makeSkijaStatefulWidget

type TextWidget[Widget[_]] = [Event] => (String, SkijaTextStyle) => Widget[Event]

def makeSkijaTextWidget[F[_] : Monad, PlaceError](
                                                   globalShaper: Shaper,
                                                   ffi: ForeighFunctionInterface[F],
                                                   cache : Cache[F, (String, SkijaTextStyle, Option[Pixel]), Sized[Pixel, SkijaPlacedText]]
): TextWidget[SkijaWidget[F, Pixel, PlaceError, *, SkijaDownEvent]] =
  [Event] => (text: String, style: SkijaTextStyle) => 
    skijaText(ffi, skijaSizeText(ffi, globalShaper, cache), text, style)
end makeSkijaTextWidget
