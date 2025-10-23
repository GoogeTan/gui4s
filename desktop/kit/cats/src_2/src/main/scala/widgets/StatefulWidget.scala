package gui4s.desktop.kit.cats
package widgets

import effects.*
import cats.data.*
import cats.effect.IO
import glfw4s.core.types.*
import gui4s.desktop.widget.library.StatefulWidget

def statefulWidget : StatefulWidget[DesktopWidget, Update, [State] =>> State => RecompositionReaction] =
  gui4s.desktop.kit.common.widgets.statefulWidget[EitherT[IO, GlfwError, *]]
end statefulWidget
