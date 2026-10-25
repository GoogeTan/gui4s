package gui4s.desktop.kit.widgets.decorator

import gui4s.core.widget.library.decorator.Decorator
import gui4s.desktop.kit.effects.Place.given
import gui4s.desktop.kit.effects.RecompositionReaction
import gui4s.desktop.kit.widgets.DesktopWidget

def destructorWidget[Event](
  name : String,
  destructor : RecompositionReaction
) : Decorator[DesktopWidget[Event]] =
  gui4s.desktop.widget.library.decorator.destructorWidget(
    name,
    destructor
  )
end destructorWidget
