package gui4s.android.kit.effects

import gui4s.core.layout.Sized

type InnerPlace[T] = Sized[Float, T]

object InnerPlace:
  export gui4s.core.layout.Sized.given
end InnerPlace
