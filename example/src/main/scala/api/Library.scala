package me.katze.gui4s.example
package api

import api.exported.{*, given}

import catnip.FFI
import catnip.syntax.all.{*, given}
import cats.Monad
import cats.data.NonEmptyList
import cats.syntax.all.*
import io.github.humbleui.skija.shaper.Shaper
import me.katze.gui4s.layout.Sized
import me.katze.gui4s.skija.{SkijaPlacedText, SkijaTextStyle}
import me.katze.gui4s.widget.library.StatefulWidget
import me.katze.gui4s.widget.{EventReaction, Path}
import scalacache.Cache

import scala.language.experimental.namedTypeArguments
import scala.reflect.Typeable

def makeSkijaStatefulWidget[F[_]: Monad, PlaceError](
  typecheckError: (Any, Path) => PlaceError
): StatefulWidget[
  SkijaWidget[F, Float, PlaceError, *, SkijaDownEvent],
  Nothing
] =
  new StatefulWidget[
    SkijaWidget[F, Float, PlaceError, *, SkijaDownEvent],
    Nothing
  ]:
    override def apply[State : Typeable, Event, ChildEvent](
                                                              name: String,
                                                              initialState: State,
                                                              eventHandler: (State, NonEmptyList[ChildEvent]) => EventReaction[State, Event, Nothing],
                                                              body: State => SkijaWidget[F, Float, PlaceError, *, SkijaDownEvent][ChildEvent]
                                                            ): SkijaWidget[F, Float, PlaceError, *, SkijaDownEvent][Event] =

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
  ffi: FFI[F],
  cache : Cache[F, (String, SkijaTextStyle, Option[Float]), Sized[Float, SkijaPlacedText]]
): TextWidget[SkijaWidget[F, Float, PlaceError, *, SkijaDownEvent]] =
  [Event] => (text: String, style: SkijaTextStyle) => skijaText(globalShaper, ffi, skijaSizeText(cache)(ffi, _, globalShaper, _), text, style)