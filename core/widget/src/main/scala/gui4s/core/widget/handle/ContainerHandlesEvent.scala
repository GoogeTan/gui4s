package gui4s.core.widget
package handle

import catnip.Zip
import catnip.Zip.zip
import catnip.syntax.all.*
import catnip.syntax.list.traverseUntil
import cats.syntax.all.*
import cats.{Foldable, Functor, Monad, Traverse}
import gui4s.core.widget.free.AsFree

final case class LayoutIncrementalWidget[Widget, Place[_], Meta](
  oldPlacedWidget : (Widget, Meta),
  newWidget : Option[Place[Widget]]
):
  def widget : Widget = oldPlacedWidget._1
  def meta : Meta = oldPlacedWidget._2


/**
 * Тип функции, описывающей установку множества виджетов в контейнер.
 * Принимает множество свободных детей и возвращает свободное множество размещенных виджетов.
 *
 * @tparam Place Эффект установки виджета
 * @tparam Collection Множества виджетов. Это может быть List, если это правило установки линейного контейнера или Id, если правило только для одного виджета.:
 * @tparam Widget Размещенный виджет
 * @tparam Meta Вспомогательные данные об результатах установки(например, координаты). TODO может, можно обобщить на произвольную комонаду
 * @todo update me
 */
trait Layout[Place[_], Collection[_], FreeWidget, IncrementalFreeWidget, PlacedWidget]:
  def place(widgets : Collection[FreeWidget]) : Place[Collection[PlacedWidget]]

  def placeIncrementally(
    widgets : Collection[IncrementalFreeWidget]
  ) : Place[Collection[PlacedWidget]]
end Layout

def containerHandlesEvent[
  Update[_] : Monad,
  Place[_] : Functor,
  Collection[_] : {Foldable, Functor, Zip},
  Widget,
  HandlableEvent,
  Meta
](
  childrenHandleEvent :
    HandlesEvent[Collection[(Widget, Meta)], HandlableEvent, Update[Collection[Option[Place[Widget]]]]],
  placeIncrementally : Collection[((Widget, Meta), Option[Place[Widget]])] => Place[Collection[(Widget, Meta)]]
) : HandlesEventF[
  Collection[(Widget, Meta)],
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

type TraverseChildrenOrdered[Update[_], Place[_], Collection[_], Widget, Meta] =
  Collection[(Widget, Meta)] => (f: Collection[(Widget, Meta)] => Update[Collection[Option[Place[Widget]]]]) => Update[Collection[Option[Place[Widget]]]]

def childrenHandleEvent[
  Collection[_] : {Traverse, Zip as Z},
  Update[_] : Monad as UM,
  Place[_] : Functor,
  Widget,
  HandlableEvent,
  Meta,
](
    widgetHandlesEvent : HandlesEvent[Widget, HandlableEvent, Update[Option[Place[Widget]]]],
    widgetAsFree : AsFree[Widget, Place[Widget]],
    isEventConsumed : Update[Boolean],
    adjustUpdateToMeta : [T] => (Update[T], Meta) => Update[T],
    traverseContainerOrdered : TraverseChildrenOrdered[Update, Place, Collection, Widget, Meta]
) : HandlesEvent[Collection[(Widget, Meta)], HandlableEvent, Update[Collection[Option[Place[Widget]]]]] =
  def updateChildren(children: Collection[(Widget, Meta)], pathToParent: Path, event: HandlableEvent): Update[Collection[Option[Place[Widget]]]] =
    traverseUntil(
      original = children,
      main = (currentChild, currentMeta) => UM.product(
        adjustUpdateToMeta(widgetHandlesEvent(currentChild, pathToParent, event), currentMeta),
        isEventConsumed,
      ),
      afterAll = (currentChild, _) => None.pure[Update],
    )
  end updateChildren

  def updateChildrenOrdered(children : Collection[(Widget, Meta)], pathToParent : Path, event : HandlableEvent) : Update[Collection[Option[Place[Widget]]]] =
    traverseContainerOrdered(children)(
      orderedChildren => updateChildren(orderedChildren, pathToParent, event)
    )
  end updateChildrenOrdered

  updateChildrenOrdered
end childrenHandleEvent
