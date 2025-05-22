package me.katze.gui4s.widget
package refactor.handle

import refactor.Container
import refactor.free.AsFree

import catnip.syntax.list.orderedListProcessing
import cats.syntax.all.*
import cats.{Functor, Monad}

type Layout[Place[_], Widget] = List[Place[Widget]] => Place[List[Widget]]

def containerHandlesEvent[
  Update[+_] : Monad,
  Place[_] : Functor,
  Widget,
  HandlableEvent
](
  childrenHandleEvent : HandlesEvent[List[Widget], HandlableEvent, Update[List[Place[Widget]]]],
) : HandlesEvent[
  Container[Widget, Layout[Place, Widget]],
  HandlableEvent,
  Update[Place[Container[Widget, Layout[Place, Widget]]]]
] =
  (self, pathToParent, event) =>
    childrenHandleEvent(self.children, pathToParent, event)
      .map(childrenUnplaced =>
        self.layout(childrenUnplaced)
          .map(childrenPlaced => Container(childrenPlaced, self.layout))
      )

def childrenHandleEvent[
  Update[+_] : Monad,
  Place[_] : Functor,
  Widget,
  HandlableEvent
](
    widgetHandlesEvent : HandlesEvent[Widget, HandlableEvent, Update[Place[Widget]]],
    widgetAsFree : AsFree[Widget, Place[Widget]],
    order : Widget => Int,
    eventConsumed : Update[Boolean]
) : HandlesEvent[List[Widget], HandlableEvent, Update[List[Place[Widget]]]] =
  def updateChildrenOrdered(children : List[Widget], pathToParent : Path, event : HandlableEvent) : Update[List[Place[Widget]]] =
    orderedListProcessing(children)(order)(updateChildren(_, pathToParent, event))
  end updateChildrenOrdered

  def updateChildren(children : List[Widget], pathToParent : Path, event : HandlableEvent) : Update[List[Place[Widget]]] =
    children match
      case currentChild :: remainingChildren =>
        for
          widget <- widgetHandlesEvent(currentChild, pathToParent, event)
          shouldContinue <- eventConsumed
          remaining <-
            if shouldContinue then
              updateChildren(remainingChildren, pathToParent, event)
            else
              remainingChildren.map(widgetAsFree.asFree).pure[Update]
        yield widget :: remaining
      case Nil => Nil.pure[Update]
      
  updateChildrenOrdered    


