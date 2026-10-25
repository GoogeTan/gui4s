package gui4s.desktop.kit
package effects

import gui4s.core.geometry.Rect
import gui4s.core.layout.Sized

type Situated[T] = Sized[Rect[Float], T]

object Situated:
  export gui4s.core.layout.Sized.given
end Situated
