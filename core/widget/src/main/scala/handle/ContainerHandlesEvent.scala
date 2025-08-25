package gui4s.core.widget
package handle

import free.AsFree

import catnip.syntax.additional.*
import catnip.syntax.list.traverseUntil
import cats.syntax.all.*
import cats.{Functor, Monad, Order, Traverse}

type Layout[Place[_], Container[_], Widget, Meta] = Container[Place[Widget]] => Place[Container[(Widget, Meta)]]

def containerHandlesEvent[
  Update[_] : Monad,
  Place[_] : Functor,
  C[_],
  Widget,
  HandlableEvent,
  Meta
](
    childrenHandleEvent : HandlesEvent[C[(Widget, Meta)], HandlableEvent, Update[C[Place[Widget]]]],
) : HandlesEventF[
  Container[C[(Widget, Meta)], Layout[Place, C, Widget, Meta]],
  HandlableEvent,
  Update * Place
] =
  (self, pathToParent, event) =>
    childrenHandleEvent(self.children, pathToParent, event)
      .map(childrenUnplaced =>
        self.layout(childrenUnplaced)
          .map(childrenPlaced => Container(childrenPlaced, self.layout))
      )
end containerHandlesEvent

def childrenHandleEvent[
  C[_] : Traverse,
  Update[_] : Monad as UM,
  Place[_] : Functor,
  Widget,
  HandlableEvent,
  Meta : Order,
](
    widgetHandlesEvent : HandlesEvent[Widget, HandlableEvent, Update[Place[Widget]]],
    widgetAsFree : AsFree[Widget, Place[Widget]],
    isEventConsumed : Update[Boolean],
    adjustUpdateToMeta : [T] => (Update[T], Meta) => Update[T],
    updateListOrdered : [A : Order, B] => (list: C[A]) => (f: C[A] => Update[C[B]]) => Update[C[B]]
) : HandlesEvent[C[(Widget, Meta)], HandlableEvent, Update[C[Place[Widget]]]] =
  def updateChildren(children: C[(Widget, Meta)], pathToParent: Path, event: HandlableEvent): Update[C[Place[Widget]]] =
    traverseUntil(
      original = children,
      main = (currentChild, currentMeta) => UM.product(
        isEventConsumed,
        adjustUpdateToMeta(widgetHandlesEvent(currentChild, pathToParent, event), currentMeta)
      ),
      afterAll = (currentChild, _) => widgetAsFree(currentChild).pure[Update]
    )
  end updateChildren

  def updateChildrenOrdered(children : C[(Widget, Meta)], pathToParent : Path, event : HandlableEvent) : Update[C[Place[Widget]]] =
    given Order[(Widget, Meta)] = Order.by(_._2)
    updateListOrdered(children)(
      orderedChildren => updateChildren(orderedChildren, pathToParent, event)
    )
  end updateChildrenOrdered

  updateChildrenOrdered
end childrenHandleEvent
