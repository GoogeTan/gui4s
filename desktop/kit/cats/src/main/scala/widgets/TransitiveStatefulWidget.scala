package gui4s.desktop.kit.cats
package widgets

import effects.Update
import cats.effect.IO

import gui4s.desktop.widget.library.TransitiveStatefulWidget

def transitiveStatefulWidget: TransitiveStatefulWidget[DesktopWidget, Update] =
  gui4s.desktop.kit.widgets.transitiveStatefulWidget[IO]
end transitiveStatefulWidget 
