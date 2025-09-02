package gui4s.desktop.kit
package widgets.decorator

import effects.*
import effects.Place.given
import effects.Update.given
import effects.RecompositionReaction.given
import cats.*
import widgets.*

import gui4s.core.widget.Path
import cats.effect.std.Supervisor
import gui4s.desktop.widget.library.*
import gui4s.desktop.widget.library.launchedEffect as genericLaunchedEffect

import scala.reflect.Typeable

@SuppressWarnings(Array("org.wartremover.warts.ToString"))
def launchedEffect[IO[_] : MonadThrow as MT, Event, Key : Typeable](supervisor : Supervisor[IO]) : LaunchedEffectWidget[DesktopWidget[IO, Event], Key, Path => IO[Unit]] =
  val lew : LaunchedEffectWidget[
    DesktopWidget[IO, Event],
    Key,
    Path => RecompositionReaction[IO]
  ] = genericLaunchedEffect[
    UpdateC[IO, Event],
    PlaceC[IO],
    Draw[IO],
    RecompositionReaction[IO],
    DownEvent,
    Key
  ](
    [T] => (path : Path, value : Any) => OuterPlace.raiseError(new Exception("Key has changed type at " + path.toString + " value found " + value.toString)),
    (valueFound : Any) => RecompositionReaction.lift[IO, Any](
      MT.raiseError[Any](Exception("Key changed the type: " + valueFound.toString))
    ),
  )
  (name, child, key, task) =>
    lew(name, child, key, path =>
      RecompositionReaction.lift(
        supervisor.supervise(task(path))
      )
    )
end launchedEffect
