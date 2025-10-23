package gui4s.desktop.kit.cats
package widgets

import cats.data.EitherT
import cats.effect.IO
import glfw4s.core.types.GlfwError 

type DesktopPlacedWidget[Event] = gui4s.desktop.kit.common.widgets.DesktopPlacedWidget[EitherT[IO, GlfwError, *], Event]
type DesktopWidget[Event] = gui4s.desktop.kit.common.widgets.DesktopWidget[EitherT[IO, GlfwError, *], Event]
