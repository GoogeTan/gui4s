package me.katze.gui4s.example
package api.widget

import catnip.ForeighFunctionInterface
import catnip.syntax.all.{*, given}
import cats.kernel.Monoid
import cats.{Id, Monad, Order}
import me.katze.gui4s.example.api.effects.{*, given}
import me.katze.gui4s.geometry.{InfinityOr, Point3d, Rect}
import me.katze.gui4s.layout.{Sized, SizedT}
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
    Rect[InfinityOr[Float]],
    Float,
    PlaceError,
    Widget[
      SkijaUpdateT[IO, Float, Clip, UpdateError, Event],
      SkijaPlaceT[IO, Rect[InfinityOr[Float]], Float, PlaceError],
      SkijaDraw[IO],
      RecompositionReaction,
      DownEvent,
    ]
  ],
  Paddings[Float]
] =
  type Widget_ = SkijaPlace[
    IO,
    Rect[InfinityOr[Float]],
    Float,
    PlaceError,
    Widget[
      SkijaUpdateT[IO, Float, Clip, UpdateError, Event],
      SkijaPlaceT[IO, Rect[InfinityOr[Float]], Float, PlaceError],
      SkijaDraw[IO],
      RecompositionReaction,
      DownEvent,
    ]
  ]
  gapPaddingWidget[
    SkijaUpdateT[IO, Float, Clip, UpdateError, Event],
    SkijaOuterPlaceT[IO, Rect[InfinityOr[Float]], PlaceError],
    SizedT[Float],
    SkijaDraw[IO],
    RecompositionReaction,
    DownEvent,
    Paddings[Float],
  ](
    paddings => [T] => place =>
      SkijaOuterPlace.withBounds[IO, Rect[InfinityOr[Float]], PlaceError, Sized[Float, T]](place, _.cut(paddings.horizontalLength, paddings.verticalLength, _.minus[Float](_))),
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
    Rect[InfinityOr[Float]],
    Float,
    PlaceError,
    Widget[
      SkijaUpdateT[IO, Float, Clip, UpdateError, Event],
      SkijaPlaceT[IO, Rect[InfinityOr[Float]], Float, PlaceError],
      SkijaDraw[IO],
      RecompositionReaction,
      DownEvent,
    ]
  ],
  Paddings[Padding[Float]],
] =
  paddingWidget[
    SkijaUpdateT[IO, Float, Clip, UpdateError, Event],
    SkijaOuterPlaceT[IO, Rect[InfinityOr[Float]], PlaceError],
    SkijaPlaceT[IO, Rect[InfinityOr[Float]], Float, PlaceError],
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
        SkijaPlaceT[IO, Rect[InfinityOr[Float]], Float, PlaceError],
        SkijaDraw[IO],
        RecompositionReaction,
        DownEvent,
      ],
      InfinityOr[Float],
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
        Id,
        Rect[InfinityOr[Float]]
      ](
        ffi,
        [A : Order, B] => (v : Id[A]) => (f : Id[A] => SkijaUpdate[IO, Float, Clip, UpdateError, Event, Id[B]]) => f(v),
      ),
      _.minus(_)
    ),
    placeError
  )
end padding
