package gui4s.android.kit.widgets.decorator

import gui4s.core.widget.library.decorator.Decorator
import gui4s.android.kit.effects.Place.given
import gui4s.android.kit.effects.RecompositionReaction
import gui4s.android.kit.widgets.AndroidWidget

def destructorWidget[Event](
  name : String,
  destructor : RecompositionReaction
) : Decorator[AndroidWidget[Event]] =
  gui4s.desktop.widget.library.decorator.destructorWidget(
    name,
    destructor
  )
end destructorWidget
