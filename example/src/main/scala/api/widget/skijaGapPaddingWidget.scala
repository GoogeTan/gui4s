package me.katze.gui4s.example
package api.widget

import catnip.ForeighFunctionInterface
import catnip.syntax.all.{*, given}
import cats.kernel.Monoid
import cats.{Id, Monad, Order}
import me.katze.gui4s.example.api.effects.{*, given}
import me.katze.gui4s.geometry.Point3d
import me.katze.gui4s.layout.Sized
import me.katze.gui4s.skija.{SkijaDraw, drawAt}
import me.katze.gui4s.widget.Path
import me.katze.gui4s.widget.library.{Widget, WidgetHandlesEvent}
import me.katze.gui4s.widget.library.decorator.{*, given}

import scala.math.Fractional.Implicits.*

def gapPadding[
  IO[_] : Monad,
  RecompositionReaction,
  DownEvent,
  UpdateError,
  PlaceError,
  Clip,
  Event
](
  ffi : ForeighFunctionInterface[IO],
) : PaddingWidget[
  SkijaPlace[
    IO,
    Float,
    PlaceError,
    Widget[
      SkijaUpdateT[IO, Float, Clip, UpdateError, Event],
      SkijaPlaceT[IO, Float, PlaceError],
      SkijaDraw[IO],
      RecompositionReaction,
      DownEvent,
    ]
  ],
  Paddings[Float]
] =
  type Widget_ = SkijaPlace[
    IO,
    Float,
    PlaceError,
    Widget[
      SkijaUpdateT[IO, Float, Clip, UpdateError, Event],
      SkijaPlaceT[IO, Float, PlaceError],
      SkijaDraw[IO],
      RecompositionReaction,
      DownEvent,
    ]
  ]
  gapPaddingWidget[
    SkijaUpdateT[IO, Float, Clip, UpdateError, Event],
    SkijaOuterPlaceT[IO, Float, PlaceError],
    Sized[Float, *],
    SkijaDraw[IO],
    RecompositionReaction,
    DownEvent,
    Paddings[Float],
  ](
    paddings => [T] => place =>
      SkijaOuterPlace.withBounds[IO, Float, PlaceError, Sized[Float, T]](place, _.cut(paddings.horizontalLength, paddings.verticalLength)),
    paddings => update => (path, event) =>
          SkijaUpdate.withCoordinates[IO, Float, Clip, UpdateError, Event, Widget_](update(path, event))(_ + new Point3d(paddings.topLeftCornerShift)),
    paddings => draw => drawAt(ffi, draw.value, paddings.left, paddings.top),
  )
end gapPadding

def padding[
  IO[_] : Monad as M,
  RecompositionReaction : Monoid,
  DownEvent,
  UpdateError,
  PlaceError,
  Clip,
  Event
](
  ffi : ForeighFunctionInterface[IO],
  placeError : PlaceError,
) : PaddingWidget[
  SkijaPlace[
    IO,
    Float,
    PlaceError,
    Widget[
      SkijaUpdateT[IO, Float, Clip, UpdateError, Event],
      SkijaPlaceT[IO, Float, PlaceError],
      SkijaDraw[IO],
      RecompositionReaction,
      DownEvent,
    ]
  ],
  Paddings[Padding[Float]],
] =
  paddingWidget[
    SkijaUpdateT[IO, Float, Clip, UpdateError, Event],
    SkijaOuterPlaceT[IO, Float, PlaceError],
    SkijaPlaceT[IO, Float, PlaceError],
    SkijaDraw[IO],
    RecompositionReaction,
    DownEvent,
    Float,
    PlaceError,
  ](
    gapPadding(ffi),
    skijaLinearContainer[
      IO,
      Widget[
        SkijaUpdateT[IO, Float, Clip, UpdateError, Event],
        SkijaPlaceT[IO, Float, PlaceError],
        SkijaDraw[IO],
        RecompositionReaction,
        DownEvent,
      ],
      Float,
      PlaceError,
      Id,
    ](
      skijaContainer[
        IO,
        Clip,
        UpdateError,
        PlaceError,
        Event,
        DownEvent,
        RecompositionReaction,
        Id
      ](
        ffi,
        [A : Order, B] => (v : Id[A]) => (f : Id[A] => SkijaUpdate[IO, Float, Clip, UpdateError, Event, Id[B]]) => f(v),
      ),
      [A, B] => (a, b) => (a, b)
    ),
    placeError
  )
end padding
