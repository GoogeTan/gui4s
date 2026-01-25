package gui4s.desktop.kit
package effects

import gui4s.core.layout.Sized

type Situated[T] = Sized[Float, T]

object Situated:
  export gui4s.core.layout.Sized.given
end Situated
