package gui4s.desktop.kit.cats
package widgets

import effects.Update
import cats.data.EitherT
import cats.effect.IO
import glfw4s.core.types.GlfwError
import gui4s.desktop.widget.library.TransitiveStatefulWidget

def transitiveStatefulWidget: TransitiveStatefulWidget[DesktopWidget, Update] =
  gui4s.desktop.kit.common.widgets.transitiveStatefulWidget[EitherT[IO, GlfwError, *]]
end transitiveStatefulWidget 
