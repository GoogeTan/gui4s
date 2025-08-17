package me.katze.gui4s.example
package api.widget

import api.effects.{*, given}

import cats.effect.std.Supervisor
import cats.{Applicative, Functor}
import me.katze.gui4s.widget.library.{LaunchedEffectWidget, Widget}
import me.katze.gui4s.widget.{Path, library}

import scala.reflect.Typeable

def skijaLaunchedEffect[
  IO[_] : Applicative,
  Update[_] : Applicative,
  Place[_] : Functor,
  Draw,
  DownEvent,
  Key : Typeable
](
  supervisor : Supervisor[IO],
  keyChangedTheTypePlaceError : [T] => (Path, Any) => Place[T],
  keyChangedTheTypeError : Any => SkijaRecomposition[IO],
) : LaunchedEffectWidget[Place[Widget[Update, Place, Draw, SkijaRecomposition[IO], DownEvent]], Key, Path => IO[Unit]] =
  val lew : LaunchedEffectWidget[
    Place[Widget[Update, Place, Draw, SkijaRecomposition[IO], DownEvent]],
    Key,
    Path => SkijaRecomposition[IO]
  ] = library.launchedEffect(
    keyChangedTheTypePlaceError,
    keyChangedTheTypeError
  )
  (name, child, key, task) =>
    lew(name, child, key, path =>
      SkijaRecomposition.lift(
        supervisor.supervise(task(path))
      )
    )
end skijaLaunchedEffect


