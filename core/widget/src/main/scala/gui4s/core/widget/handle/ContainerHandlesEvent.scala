package gui4s.core.widget
package handle

import catnip.Zip
import catnip.Zip.zip
import catnip.syntax.all.*
import cats.syntax.all.*
import cats.{Foldable, Functor, Monad, Traverse}

def containerHandlesEvent[
  Update[_] : Monad,
  Place[_] : Functor,
  Collection[_] : {Foldable, Functor, Zip},
  FreeWidget,
  PositionedWidget
](
  childrenHandleEvent :
    HandlesEvent_[Collection[PositionedWidget], Update[Collection[Option[FreeWidget]]]],
  placeIncrementally : Collection[(PositionedWidget, Option[FreeWidget])] => Place[Collection[PositionedWidget]]
) : HandlesEventF_[
  Collection[PositionedWidget],
  Update * Option * Place
] =
  children =>
    childrenHandleEvent(children)
      .map(childrenUnplaced =>
        if childrenUnplaced.forall(_.isEmpty) then
          None
        else
          Some(placeIncrementally(children.zip(childrenUnplaced)))
      )
end containerHandlesEvent

type TraverseChildrenOrdered[Update[_], Collection[_], FreeChild, PositionedWidget] =
  Collection[PositionedWidget]
      => (f: Collection[PositionedWidget] => Update[Collection[Option[FreeChild]]])
      => Update[Collection[Option[FreeChild]]]

def childrenHandleEvent[
  Collection[_] : {Traverse, Zip as Z},
  Update[_] : Monad as UM,
  FreeWidget,
  PositionedWidget,
](
    widgetHandlesEvent : HandlesEvent_[PositionedWidget, Update[Option[FreeWidget]]],
    traverseContainerOrdered : TraverseChildrenOrdered[Update, Collection, FreeWidget, PositionedWidget]
) : HandlesEvent_[Collection[PositionedWidget], Update[Collection[Option[FreeWidget]]]] =
  children =>
    traverseContainerOrdered(children)(
      orderedChildren =>
        orderedChildren.traverse(widgetHandlesEvent)
    )
end childrenHandleEvent