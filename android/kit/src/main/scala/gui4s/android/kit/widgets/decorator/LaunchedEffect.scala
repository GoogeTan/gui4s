package gui4s.android.kit.widgets.decorator

import gui4s.core.widget.Path
import gui4s.android.kit.effects.*
import gui4s.android.kit.effects.Place.given
import gui4s.android.kit.effects.RecompositionReaction.given
import gui4s.android.kit.widgets.AndroidWidget
import gui4s.desktop.widget.library.{launchedEffect as genericLaunchedEffect, *}

import scala.reflect.Typeable

@SuppressWarnings(Array("org.wartremover.warts.ToString"))
def launchedEffect[IO[_] : MonadThrow as MT, Event, Key : Typeable](supervisor : Supervisor[IO]) : LaunchedEffectWidget[AndroidWidget[IO, Event], Key, Path => IO[Unit]] =
  val lew : LaunchedEffectWidget[
    AndroidWidget[IO, Event],
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
    Place.addNameToPath[IO]
  )
  (name, child, key, task) =>
    lew(name, child, key, path =>
      RecompositionReaction.lift(
        supervisor.supervise(task(path))
      )
    )
end launchedEffect
