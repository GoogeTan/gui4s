package gui4s.core.widget
package handle

import catnip.Zip
import catnip.Zip.zip
import catnip.syntax.all._
import catnip.syntax.list.traverseUntil
import cats.Foldable
import cats.Functor
import cats.Monad
import cats.Traverse
import cats.syntax.all._

def containerHandlesEvent[
  Update[_] : Monad,
  Place[_] : Functor,
  Collection[_] : {Foldable, Functor, Zip},
  FreeWidget,
  PositionedWidget,
  HandlableEvent
](
  childrenHandleEvent :
    HandlesEvent[Collection[PositionedWidget], HandlableEvent, Update[Collection[Option[FreeWidget]]]],
  placeIncrementally : Collection[(PositionedWidget, Option[FreeWidget])] => Place[Collection[PositionedWidget]]
) : HandlesEventF[
  Collection[PositionedWidget],
  HandlableEvent,
  Update * Option * Place
] =
  (children, pathToParent, event) =>
    childrenHandleEvent(children, pathToParent, event)
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
  EnvironmentalEvent,
](
    widgetHandlesEvent : HandlesEvent[PositionedWidget, EnvironmentalEvent, Update[Option[FreeWidget]]],
    isEventConsumed : Update[Boolean],
    traverseContainerOrdered : TraverseChildrenOrdered[Update, Collection, FreeWidget, PositionedWidget]
) : HandlesEvent[Collection[PositionedWidget], EnvironmentalEvent, Update[Collection[Option[FreeWidget]]]] =
  (children, pathToParent, event) =>
    traverseContainerOrdered(children)(
      orderedChildren => traverseUntil(
        original = orderedChildren,
        main = currentChild => UM.product(
          widgetHandlesEvent(currentChild, pathToParent, event),
          isEventConsumed,
        ),
        afterAll = _ => None.pure[Update],
      )
    )
end childrenHandleEvent
