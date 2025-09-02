package gui4s.desktop.kit.zio
package widgets

import effects.*

import gui4s.desktop.widget.library.StatefulWidget
import zio.*
import zio.interop.catz.*

def statefulWidget : StatefulWidget[DesktopWidget, Update, [State] =>> State => RecompositionReaction] =
  gui4s.desktop.kit.common.widgets.statefulWidget[Task]
end statefulWidget
