package gui4s.desktop.kit
package widgets.decorator

import scala.reflect.Typeable

import cats.*
import cats.effect.*
import cats.effect.std.Supervisor

import gui4s.core.widget.Path

import gui4s.desktop.kit.effects.*
import gui4s.desktop.kit.effects.Place.given
import gui4s.desktop.kit.effects.RecompositionReaction.given
import gui4s.desktop.kit.widgets.DesktopWidget
import gui4s.desktop.widget.library.{launchedEffect as genericLaunchedEffect, *}

@SuppressWarnings(Array("org.wartremover.warts.ToString"))
def launchedEffect[Event, Key : Typeable](supervisor : Supervisor[IO]) : LaunchedEffectWidget[DesktopWidget[Event], Key, Path => IO[Unit]] =
  val lew : LaunchedEffectWidget[
    DesktopWidget[Event],
    Key,
    Path => RecompositionReaction
  ] = genericLaunchedEffect[
    UpdateC[Event],
    Place,
    Draw,
    RecompositionReaction,
    Key
  ](
    [T] => (value : Any) =>
      Place.raiseError[Throwable, T](path => new Exception("Key has changed type at " + path.toString + " value found " + value.toString)),
    (valueFound : Any) => RecompositionReaction.lift[Any](
      IO.raiseError[Any](Exception("Key changed the type: " + valueFound.toString))
    ),
    Place.addNameToPath
  )
  (name, child, key, task) =>
    lew(name, child, key, path =>
      RecompositionReaction.lift(
        supervisor.supervise(task(path))
      )
    )
end launchedEffect
