package gui4s.desktop.kit.cats
package widgets

import effects.*
import cats.effect.IO
import gui4s.desktop.widget.library.StatefulWidget

def statefulWidget : StatefulWidget[DesktopWidget, Update, [State] =>> State => RecompositionReaction] =
  gui4s.desktop.kit.widgets.statefulWidget[IO]
end statefulWidget
