package gui4s.desktop.widget.library

import cats.Functor
import cats.syntax.all.*

import gui4s.core.widget.Path
import gui4s.core.widget.StateTree
import gui4s.core.widget.draw.Drawable
import gui4s.core.widget.free.AsFree
import gui4s.core.widget.handle.HandlesEvent_
import gui4s.core.widget.merge.MergesWithOldStates
import gui4s.core.widget.recomposition.ReactsOnRecomposition
import gui4s.core.widget.state.HasInnerStates

/**
 * Представляет собой общее представление любого (возможно, только десктопного) размещенного на экране виджета.
 *
 * Всякий виджет изначально представляется в виде Place[Widget]. Такой виджет называется свободным.
 *
 * Виджет может иметь в себе внутреннее состояние, в таком случае он должен иметь имя. 
 *
 * Состояния всех виджетов образуют подвешенное дерево. Вершинами в нем являются именованные состояния, и одно находится в под-дереве другого,
 * если зависит от него.
 * Путем виджета называется путь от корня ближайшего родительского состояния.
 * Таким образом, если виджет с внутренним состоянием содержит в себе под-виджет(ы), зависящие от его внутреннего состояния,
 * то этот виджет должен при вызове всех методов дочерних виджетов добавлять своё имя к пути.
 *
 * Виджет должен сохранять все свои под-состояния (он может сам не иметь состояния, но дочерние виджеты могут иметь оные) в методе
 * innerStates с их соответствующими именами.
 *
 *
 *
 * Виджету необходимо знать свой путь для выполнения ввода-вывода. Ввод-вывод для общения с виджетом может только порождать
 * внешние события. Чтобы виджеты могли отличать, кому предназначены сообщения ввода-вывода, используются пути.
 *
 * @tparam Update Эффект реакции на событие
 * @tparam Place Эффект установки на экран
 * @tparam Draw Графическое представление (например, IO[Unit], рисующий при помощи open gl, или текст на html).
 * @tparam RecompositionReaction Реакция на рекомпозицию.
 * 
 * @param asFree Возвращает виджет в свободное состояние до установки на экран.
 * @param draw Возвращает графическое представление виджета.
 * @param handleEvent Обрабатывает событие, возвращает свободный виджет в эффекте обновления
 * @param innerStates Сохраняет состояние виджета в дереве состояний.
 * @param mergeWithOldState Восстанавливает состояние из дерева состояний
 * @param reactOnRecomposition Вызывается после обновления виджета.
 */
enum Widget[
  Update[_],
  Place[_],
  Draw,
  RecompositionReaction
](
  val asFree : Place[Widget[Update, Place, Draw, RecompositionReaction]],
  val draw : Draw,
  val handleEvent :
    Update[
      Option[
        Place[
          Widget[Update, Place, Draw, RecompositionReaction]
        ]
      ]
    ],
  val mergeWithOldState : (oldState:  Map[String, StateTree[RecompositionReaction]]) => Option[Place[Widget[Update, Place, Draw, RecompositionReaction]]],
  val reactOnRecomposition : (pathToParent: Path, oldStates: Map[String, StateTree[RecompositionReaction]]) => RecompositionReaction,
  val innerStates: Map[String, StateTree[RecompositionReaction]]
):
  case ValueWrapper[
    T, Update_[_] : Functor, Place_[_] : Functor, Draw_, RecompositionReaction_
  ](
    valueToDecorate: T,
    valueAsFree: AsFree[T, Place_[T]],
    valueIsDrawable: Drawable[T, Draw_],
    valueHandlesEvent: T => Update_[Option[Place_[T]]],
    valueMergesWithOldState: MergesWithOldStates[T, RecompositionReaction_, Option[Place_[T]]],
    valueReactsOnRecomposition: ReactsOnRecomposition[T, RecompositionReaction_],
    valueHasInnerState: HasInnerStates[T, RecompositionReaction_],
  ) extends Widget[Update_, Place_, Draw_, RecompositionReaction_](
    valueAsFree(valueToDecorate).map(ValueWrapper(_, valueAsFree, valueIsDrawable, valueHandlesEvent, valueMergesWithOldState, valueReactsOnRecomposition, valueHasInnerState)),
    valueIsDrawable(valueToDecorate),
    valueHandlesEvent(valueToDecorate).map(
      _.map(
        _.map(ValueWrapper(_, valueAsFree, valueIsDrawable, valueHandlesEvent, valueMergesWithOldState, valueReactsOnRecomposition, valueHasInnerState))
      )
    ),
    oldStates => valueMergesWithOldState(valueToDecorate, oldStates)
      .map(
        _.map(
          ValueWrapper(_, valueAsFree, valueIsDrawable, valueHandlesEvent, valueMergesWithOldState, valueReactsOnRecomposition, valueHasInnerState)
        )
      ),
    valueReactsOnRecomposition(valueToDecorate, _, _),
    valueHasInnerState(valueToDecorate)
  )
end Widget

def widgetAsFree[
  Update[_],
  Place[_],
  Draw,
  RecompositionReaction
] : AsFree[
  Widget[Update, Place, Draw, RecompositionReaction],
  Place[Widget[Update, Place, Draw, RecompositionReaction]]
] =
  _.asFree
end widgetAsFree

def widgetIsDrawable[
  Update[_],
  Place[_],
  Draw,
  RecompositionReaction
] : Drawable[
  Widget[Update, Place, Draw, RecompositionReaction],
  Draw
] =
  _.draw
end widgetIsDrawable

def widgetHandlesEvent[
  Update[_],
  Place[_],
  Draw,
  RecompositionReaction
] : HandlesEvent_[
  Widget[Update, Place, Draw, RecompositionReaction],
  Update[
    Option[Place[Widget[Update, Place, Draw, RecompositionReaction]]]
  ]
] =
  _.handleEvent
end widgetHandlesEvent

def widgetMergesWithOldState[
  Update[_],
  Place[_],
  Draw,
  RecompositionReaction
] : MergesWithOldStates[
  Widget[Update, Place, Draw, RecompositionReaction],
  RecompositionReaction,
  Option[Place[Widget[Update, Place, Draw, RecompositionReaction]]],
] =
  _.mergeWithOldState(_)

def widgetReactsOnRecomposition[
  Update[_],
  Place[_],
  Draw,
  RecompositionReaction
] : ReactsOnRecomposition[
  Widget[Update, Place, Draw, RecompositionReaction],
  RecompositionReaction
] =
  _.reactOnRecomposition(_, _)
end widgetReactsOnRecomposition

def widgetHasInnerStates[
  Update[_],
  Place[_],
  Draw,
  RecompositionReaction
] : HasInnerStates[
  Widget[Update, Place, Draw, RecompositionReaction],
  RecompositionReaction
] =
  _.innerStates
end widgetHasInnerStates

