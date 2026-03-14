package gui4s.core.widget
package free

import cats.Functor

def containerAsFree[Place[_] : Functor, Collection[_] : Functor, PositionedWidget](
  placeIncrementally: Collection[PositionedWidget] => Place[Collection[PositionedWidget]],
) : AsFreeF[
  Collection[PositionedWidget],
  Place
] =
  children =>
    placeIncrementally(children)
end containerAsFree
