package gui4s.desktop.kit.zio
package widgets

import effects.Update

import gui4s.desktop.widget.library.TransitiveStatefulWidget
import zio.*
import zio.interop.catz.*

def transitiveStatefulWidget: TransitiveStatefulWidget[DesktopWidget, Update] =
  gui4s.desktop.kit.widgets.transitiveStatefulWidget[Task]
end transitiveStatefulWidget 
