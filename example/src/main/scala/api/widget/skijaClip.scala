package me.katze.gui4s.example
package api.widget

import catnip.ForeighFunctionInterface
import catnip.syntax.all.{*, given}
import cats.{Comonad, Functor, Monad}
import me.katze.gui4s.example.api.effects.{SkijaClip, SkijaUpdate, SkijaUpdateT, given}
import me.katze.gui4s.geometry.Rect
import me.katze.gui4s.skija.SkijaDraw
import me.katze.gui4s.widget.library.Widget
import me.katze.gui4s.widget.library.decorator.{Decorator, clipWidget}

def skijaClip[
  IO[_] : Monad,
  UpdateError,
  Place[_] : Functor,
  InnerPlace[_] : Comonad,
  RecompositionReaction,
  DownEvent,
  Event
](
  ffi : ForeighFunctionInterface[IO]
)(
  path : InnerPlace[Unit] => SkijaClip
): Decorator[Place[InnerPlace[Widget[SkijaUpdateT[IO, Float, SkijaClip, UpdateError, Event], Place * InnerPlace, SkijaDraw[IO], RecompositionReaction, DownEvent]]]] =
  clipWidget[
    SkijaUpdateT[IO, Float, SkijaClip, UpdateError, Event],
    Place,
    InnerPlace,
    SkijaDraw[IO],
    RecompositionReaction,
    DownEvent,
    SkijaClip
  ](
    [T] => (a, b) => SkijaUpdate.withClip[IO, Float, SkijaClip, UpdateError, Event, T](a, b, SkijaClip.skijaPathAt),
    SkijaClip.clipToPath[IO](ffi, _ : SkijaClip, _ : SkijaDraw[IO]),
    path,
  )
end skijaClip
