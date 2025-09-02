package gui4s.desktop.kit.zio
package widgets.decorator

import effects.*

import cats.*
import cats.effect.std.Supervisor
import gui4s.desktop.kit.zio.widgets.DesktopWidget
import gui4s.desktop.widget.library.{launchedEffect as genericLaunchedEffect, *}
import gui4s.core.widget.Path
import zio.*
import zio.interop.catz.*

import scala.reflect.Typeable

def launchedEffect[Event, Key : Typeable](supervisor : Supervisor[Task]) : LaunchedEffectWidget[DesktopWidget[Event], Key, Path => Task[Unit]] =
  gui4s.desktop.kit.widgets.decorator.launchedEffect(supervisor)
end launchedEffect
