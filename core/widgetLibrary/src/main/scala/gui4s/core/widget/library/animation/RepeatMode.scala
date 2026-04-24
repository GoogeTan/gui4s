package gui4s.core.widget.library.animation

import cats.Eq
import cats.derived.*

enum RepeatMode derives Eq:
  case Restart extends RepeatMode
  case Reverse extends RepeatMode
end RepeatMode