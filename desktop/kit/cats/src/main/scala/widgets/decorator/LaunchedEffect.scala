package gui4s.desktop.kit.cats
package widgets.decorator

import effects.*
import effects.Place.given
import effects.Update.given
import widgets.*

import cats.effect.IO
import cats.effect.std.Supervisor
import gui4s.decktop.widget.library.*
import gui4s.decktop.widget.library.launchedEffect as genericLaunchedEffect
import io.github.humbleui.skija.Path

import scala.reflect.Typeable

def launchedEffect[Event, Key : Typeable](supervisor : Supervisor[IO]) : LaunchedEffectWidget[DesktopWidget[Event], Key, Path => IO[Unit]] =
  val lew : LaunchedEffectWidget[
    DesktopWidget[Event],
    Key,
    Path => RecompositionReaction
  ] = genericLaunchedEffect(
    [T] => (path : Path, value : Any) => OuterPlace.raiseError("Key has changed type at " + path.toString + " value found " + value.toString),
    (valueFound : Any) => RecompositionReaction.lift(
      IO.raiseError(Exception("Key changed the type: " + valueFound.toString))
    ),
  )
  (name, child, key, task) =>
    lew(name, child, key, path =>
      RecompositionReaction.lift(
        supervisor.supervise(task(path))
      )
    )
end launchedEffect
