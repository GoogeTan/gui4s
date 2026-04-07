package gui4s.desktop.widget.library
package decorator

import catnip.syntax.all._
import cats.Comonad
import cats.Functor
import cats.Id
import cats.syntax.all._

import gui4s.core.widget.Path
import gui4s.core.widget.handle.HandlesEventF_
import gui4s.core.widget.library.decorator.Decorator

/**
 * Виджет, позволяющий перегрузить обновление дочернего виджета.
 * Принимает функцию, обрабатывающую обновления вместо оригинальной.
 * Надо учесть, что вызвать оригинальное обновление, если это необходимо - задача пользователя данного виджета.
 * Это полезно, если событие надо поглотить и не пропускать дальше(например, при обработке нажатия мыши).
 *
 * @tparam Update Эффект обновления виджета
 * @tparam Place Эффект установки на экран
 * @tparam Widget Размещенный виджет
 * @tparam EnvironmentalEvent Внешнее событие
 */
type UpdateDecorator[Update[_], Place[_], Widget] =
  HandlesEventF_[Widget, Update * Option * Place] => Decorator[Place[Widget]]

/**
 * Декоратор, позволяющий задекорировать обновление виджета. Отличается от [[updateDecoratorWithRect]] тем, что
 * декоратор принимает в себя функцию обновления оригинального виджета, а не его самого с его контекстом установки.
 */
def updateDecorator[
  Update[_] : Functor as UF,
  Place[_] : Functor as PF,
  Draw,
  RecompositionReaction
](
  decorator : Decorator[
    Widget[Update, Place, Draw, RecompositionReaction] =>
      Update[Option[Place[Widget[Update, Place, Draw, RecompositionReaction]]]]
  ]
): Decorator[Place[Widget[Update, Place, Draw, RecompositionReaction]]] =
  updateDecoratorWithRect[
    Update,
    Place,
    Id,
    Draw,
    RecompositionReaction
  ](
    (self : Widget[Update, Place, Draw, RecompositionReaction]) =>
      decorator(_.handleEvent)(self)
  )
end updateDecorator

/**
 * Реализация [[UpdateDecorator]]
 * @todo Может, её стоит удалить, так как она просто конкретизация метода ниже
 */
def updateDecoratorWithRect[
  Update[_] : Functor as UF,
  PlacementEffect[_] : Functor as PF,
  Situated[_] : Comonad,
  Draw,
  RecompositionReaction
] : UpdateDecorator[
  Update,
  PlacementEffect,
  Situated[Widget[Update, PlacementEffect * Situated, Draw, RecompositionReaction]]
] =
  given Functor[PlacementEffect * Situated] = nestedFunctorsAreFunctors[PlacementEffect, Situated]
  decorator => original =>
    trueUpdateDecoratorWithRect(
      original,
      decorator
    )
end updateDecoratorWithRect

type WidgetWithSituated[
  Update[_],
  PlacementEffect[_],
  Situated[_],
  Draw,
  RecompositionReaction,
] =
  Widget[
    Update, PlacementEffect * Situated, Draw, RecompositionReaction
  ]

type FreeWidgetWithSituated[
  Update[_],
  PlacementEffect[_],
  Situated[_],
  Draw,
  RecompositionReaction,
] =
  PlacementEffect[
    Situated[
      WidgetWithSituated[
        Update, PlacementEffect, Situated, Draw, RecompositionReaction
      ]
    ]
  ]

def trueUpdateDecoratorWithRect[
  OldUpdate[_],
  NewUpdate[_] : Functor as NUF,
  PlacementEffect[_] : Functor as PF,
  Situated[_] : Comonad,
  Draw,
  RecompositionReaction,
](
  original : FreeWidgetWithSituated[
    OldUpdate, PlacementEffect, Situated, Draw, RecompositionReaction
  ],
  decorator :
  Situated[
    WidgetWithSituated[OldUpdate, PlacementEffect, Situated, Draw, RecompositionReaction]
  ] =>
    NewUpdate[
      Option[
        FreeWidgetWithSituated[
          OldUpdate, PlacementEffect, Situated, Draw, RecompositionReaction
        ]
      ]
    ],
) : FreeWidgetWithSituated[
  NewUpdate, PlacementEffect, Situated, Draw, RecompositionReaction
] =
  given Functor[PlacementEffect * Situated] = nestedFunctorsAreFunctors[PlacementEffect, Situated]
  PF.map(original)(_.coflatMap(
    sizedWidget =>
      Widget.ValueWrapper[
        Situated[Widget[OldUpdate, PlacementEffect * Situated, Draw, RecompositionReaction]],
        NewUpdate,
        PlacementEffect * Situated,
        Draw,
        RecompositionReaction
      ](
        valueToDecorate = sizedWidget,
        valueAsFree = self => PF.map(self.extract.asFree)(_.coflatten),
        valueIsDrawable = _.extract.draw,
        valueHandlesEvent = self =>
          NUF.map(decorator(self))(
            _.map(
              PF.map(_)(_.coflatten)
            )
          ),
        valueMergesWithOldState = (self, oldStates) => self.extract.mergeWithOldState(oldStates).map(PF.map(_)(_.coflatten)),
        valueReactsOnRecomposition = _.extract.reactOnRecomposition(_, _),
        valueHasInnerState = _.extract.innerStates
      )
  )
  )
end trueUpdateDecoratorWithRect
