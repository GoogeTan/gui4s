package gui4s.desktop.kit.cats
package effects

import gui4s.core.layout.Sized

type InnerPlace[T] = Sized[Float, T]

object InnerPlace:
  export gui4s.core.layout.Sized.given
end InnerPlace
