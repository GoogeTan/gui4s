package gui4s.desktop.kit.zio
package widgets.decorator

import effects.*
import effects.Place.given
import effects.Update.given
import effects.RecompositionReaction.given
import widgets.*

import gui4s.core.widget.Path
import gui4s.decktop.widget.library.*
import gui4s.decktop.widget.library.launchedEffect as genericLaunchedEffect

import scala.reflect.Typeable
import zio.*
import zio.interop.catz.*

@SuppressWarnings(Array("org.wartremover.warts.All"))
def launchedEffect[Event, Key : Typeable](supervisor : Supervisor[Unit]) : LaunchedEffectWidget[DesktopWidget[Event], Key, Path => Task[Unit]] =
  val lew : LaunchedEffectWidget[
    DesktopWidget[Event],
    Key,
    Path => RecompositionReaction
  ] = genericLaunchedEffect[
    UpdateC[Event],
    Place,
    Draw,
    RecompositionReaction,
    DownEvent,
    Key
  ](
    [T] => (path : Path, value : Any) => OuterPlace.raiseError("Key has changed type at " + path.toString + " value found " + value.toString),
    (valueFound : Any) => RecompositionReaction.raiseError(
      Exception("Key changed the type: " + valueFound.toString)
    ),
  )
  (name, child, key, task) =>
    lew(name, child, key, path =>
      RecompositionReaction.lift(
        task(path).supervised(supervisor).fork
      )
    )
end launchedEffect
