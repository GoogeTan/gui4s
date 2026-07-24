package gui4s.core.widget
package free

def containerAsFree[Place[_], Collection[_], PositionedWidget](
  placeIncrementally: Collection[PositionedWidget] => Place[Collection[PositionedWidget]],
) : AsFreeF[
  Collection[PositionedWidget],
  Place
] =
  children =>
    placeIncrementally(children)
end containerAsFree
