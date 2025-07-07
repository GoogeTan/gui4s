package me.katze.gui4s.widget
package handle

import free.AsFree

import catnip.syntax.list.orderedListProcessing
import cats.syntax.all.*
import cats.{Functor, Monad}

type Layout[Place[_], Widget, Meta] = List[Place[Widget]] => Place[List[(Widget, Meta)]]

def containerHandlesEvent[
  Update[_] : Monad,
  Place[_] : Functor,
  Widget,
  HandlableEvent,
  Meta
](
  childrenHandleEvent : HandlesEvent[List[(Widget, Meta)], HandlableEvent, Update[List[Place[Widget]]]],
) : HandlesEvent[
  Container[(Widget, Meta), Layout[Place, Widget, Meta]],
  HandlableEvent,
  Update[Place[Container[(Widget, Meta), Layout[Place, Widget, Meta]]]]
] =
  (self, pathToParent, event) =>
    childrenHandleEvent(self.children, pathToParent, event)
      .map(childrenUnplaced =>
        self.layout(childrenUnplaced)
          .map(childrenPlaced => Container(childrenPlaced, self.layout))
      )
end containerHandlesEvent

def childrenHandleEvent[
  Update[_] : Monad,
  Place[_] : Functor,
  Widget,
  HandlableEvent,
  Meta : Ordering as MetaOrdering,
](
    widgetHandlesEvent : HandlesEvent[Widget, HandlableEvent, Update[Place[Widget]]],
    widgetAsFree : AsFree[Widget, Place[Widget]],
    isEventConsumed : Update[Boolean],
    adjustUpdateToMeta : [T] => (Update[T], Meta) => Update[T],
) : HandlesEvent[List[(Widget, Meta)], HandlableEvent, Update[List[Place[Widget]]]] =
  def updateChildrenOrdered(children : List[(Widget, Meta)], pathToParent : Path, event : HandlableEvent) : Update[List[Place[Widget]]] =
    given Ordering[(Widget, Meta)] = MetaOrdering.contramap(_._2)
    orderedListProcessing(children)(
      orderedChildren => updateChildren(orderedChildren, pathToParent, event)
    )
  end updateChildrenOrdered

  def updateChildren(children : List[(Widget, Meta)], pathToParent : Path, event : HandlableEvent) : Update[List[Place[Widget]]] =
    children match
      case (currentChild, currentMeta) :: remainingChildren =>
        for
          widget <- adjustUpdateToMeta(widgetHandlesEvent(currentChild, pathToParent, event), currentMeta)
          shouldNotContinue <- isEventConsumed
          remaining <-
            if shouldNotContinue then
              remainingChildren.map(_._1).map(widgetAsFree).pure[Update]
            else
              updateChildren(remainingChildren, pathToParent, event)
        yield widget :: remaining
      case Nil => 
        Nil.pure[Update]
  updateChildrenOrdered
end childrenHandleEvent
