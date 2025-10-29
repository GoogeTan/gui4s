package gui4s.desktop.kit
package widgets

import cats.MonadThrow
import gui4s.desktop.kit.effects.*
import gui4s.desktop.kit.effects.Update.given
import gui4s.desktop.widget.library.{TransitiveStatefulWidget, TransitiveStatefulWidgetFromStatefulWidget}

def transitiveStatefulWidget[IO[_] : MonadThrow]: TransitiveStatefulWidget[DesktopWidget[IO, *], Update[IO, *, *]] =
  TransitiveStatefulWidgetFromStatefulWidget[DesktopWidget[IO, *], Update[IO, *, *], [Value] =>> Value => RecompositionReaction[IO]](
    statefulWidget,
    [Event] => events => Update.emitEvents(events)
  )
end transitiveStatefulWidget