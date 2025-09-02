package gui4s.desktop.kit
package widgets

import effects.*
import effects.Update.given

import cats.MonadThrow
import gui4s.desktop.widget.library.{TransitiveStatefulWidget, TransitiveStatefulWidgetFromStatefulWidget}

def transitiveStatefulWidget[IO[_] : MonadThrow]: TransitiveStatefulWidget[DesktopWidget[IO, *], Update[IO, *, *]] =
  TransitiveStatefulWidgetFromStatefulWidget[DesktopWidget[IO, *], Update[IO, *, *], [Value] =>> Value => RecompositionReaction[IO]](
    statefulWidget,
    [Event] => events => Update.emitEvents(events)
  )
end transitiveStatefulWidget