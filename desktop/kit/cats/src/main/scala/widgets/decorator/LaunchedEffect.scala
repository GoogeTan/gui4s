package gui4s.desktop.kit.cats
package widgets.decorator

import effects.*

import cats.*
import cats.effect.std.Supervisor
import gui4s.core.widget.Path
import gui4s.desktop.kit.cats.widgets.DesktopWidget
import gui4s.desktop.widget.library.{launchedEffect as genericLaunchedEffect, *}
import cats.effect.IO
import scala.reflect.Typeable

def launchedEffect[Event, Key : Typeable](supervisor : Supervisor[IO]) : LaunchedEffectWidget[DesktopWidget[Event], Key, Path => IO[Unit]] =
  gui4s.desktop.kit.widgets.decorator.launchedEffect(supervisor)
end launchedEffect
