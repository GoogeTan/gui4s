package gui4s.core.widget
package free

import cats.Functor

def containerAsFree[Place[_] : Functor, Collection[_] : Functor, Widget, Meta](
  placeIncrementally: Collection[(Widget, Meta)] => Place[Collection[(Widget, Meta)]],
) : AsFreeF[
  Collection[(Widget, Meta)],
  Place
] =
  children =>
    placeIncrementally(children)
end containerAsFree
