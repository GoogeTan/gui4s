package gui4s.desktop.kit.cats
package widgets.decorator

import widgets.DesktopWidget

import cats.*
import cats.effect.IO
import cats.effect.std.Supervisor
import gui4s.core.widget.Path
import gui4s.desktop.widget.library.*

import scala.reflect.Typeable

def launchedEffect[Event, Key : Typeable](supervisor : Supervisor[IO]) : LaunchedEffectWidget[DesktopWidget[Event], Key, Path => IO[Unit]] =
  gui4s.desktop.kit.common.widgets.decorator.launchedEffect(supervisor)
end launchedEffect
