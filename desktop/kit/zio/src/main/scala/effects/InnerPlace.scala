package gui4s.desktop.kit.zio
package effects

type InnerPlace[T] = gui4s.desktop.kit.common.effects.InnerPlace[T]

object InnerPlace:
  export gui4s.desktop.kit.common.effects.InnerPlace.given
end InnerPlace
