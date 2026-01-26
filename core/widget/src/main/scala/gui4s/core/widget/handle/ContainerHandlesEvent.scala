package gui4s.core.widget
package handle

import catnip.syntax.additional._
import catnip.syntax.list.traverseUntil
import cats.Functor
import cats.Monad
import cats.Order
import cats.Traverse
import cats.syntax.all._

import gui4s.core.widget.free.AsFree

/**
 * Тип функции, описывающей установку множества виджетов в контейнер.
 * Принимает множество свободных детей и возвращает свободное множество размещенных виджетов.
 *
 * @tparam Place Эффект установки виджета
 * @tparam Collection Множества виджетов. Это может быть List, если это правило установки линейного контейнера или Id, если правило только для одного виджета.:
 * @tparam Widget Размещенный виджет
 * @tparam Meta Вспомогательные данные об результатах установки(например, координаты). TODO может, можно обобщить на произвольную комонаду
 */
type Layout[Place[_], Collection[_], Widget, Meta] = Collection[Place[Widget]] => Place[Collection[(Widget, Meta)]]

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
  Collection[_] : Traverse,
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
    traverseContainerOrdered : [A : Order, B] => (list: Collection[A]) => (f: Collection[A] => Update[Collection[B]]) => Update[Collection[B]]
) : HandlesEvent[Collection[(Widget, Meta)], HandlableEvent, Update[Collection[Place[Widget]]]] =
  def updateChildren(children: Collection[(Widget, Meta)], pathToParent: Path, event: HandlableEvent): Update[Collection[Place[Widget]]] =
    traverseUntil(
      original = children,
      main = (currentChild, currentMeta) => UM.product(
        isEventConsumed,
        adjustUpdateToMeta(widgetHandlesEvent(currentChild, pathToParent, event), currentMeta)
      ),
      afterAll = (currentChild, _) => widgetAsFree(currentChild).pure[Update]
    )
  end updateChildren

  def updateChildrenOrdered(children : Collection[(Widget, Meta)], pathToParent : Path, event : HandlableEvent) : Update[Collection[Place[Widget]]] =
    given Order[(Widget, Meta)] = Order.by(_._2)
    traverseContainerOrdered(children)(
      orderedChildren => updateChildren(orderedChildren, pathToParent, event)
    )
  end updateChildrenOrdered

  updateChildrenOrdered
end childrenHandleEvent
