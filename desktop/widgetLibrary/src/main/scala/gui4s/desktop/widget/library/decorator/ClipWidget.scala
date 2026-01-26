package gui4s.desktop.widget.library
package decorator

import catnip.syntax.all._
import cats.Comonad
import cats.Functor
import cats.Monad
import cats.syntax.all._

import gui4s.core.widget.library.decorator.Decorator

/**
 * Декоратор, позволяющий обрезать виджет по заданной форме.
 * В частности обрезается изображение и события мыши/курсора за пределами фигуры не будут обрабатываться(то
 * есть, если обрезать кнопку по кругу, то она не будет реагировать на клики вне круга).
 *
 * @param withClip Вычисляет данный эффект, с обрезанием событий по заданной форме
 * @param drawModifier Рисует виджет, с обрезанием изображения по заданной форме
 * @param shapeFabric Создает форму при данных размерах виджета
 */
def clipWidget[
  Update[_] : Monad as UM,
  PlacementEffect[_] : Monad as PF,
  Situated[_] : Comonad,
  Draw,
  RecompositionReaction,
  HandleableEvent,
  Shape,
](
    withClip : [T] => (Shape, Update[T]) => Update[T],
    drawModifier : (Shape, Draw) => Draw,
    shapeFabric : Situated[Unit] => Shape
) : Decorator[PlacementEffect[Situated[Widget[Update, PlacementEffect * Situated, Draw, RecompositionReaction, HandleableEvent]]]] =
  given Functor[PlacementEffect * Situated] = nestedFunctorsAreFunctors[PlacementEffect, Situated]

  placementDecorator[
    Update,
    PlacementEffect * Situated,
    PlacementEffect * Situated,
    Draw,
    RecompositionReaction,
    HandleableEvent
  ](
    PF.flatMap(_)(placement =>
      val shape = shapeFabric(placement.as(()))
      drawDecorator[
        Update,
        PlacementEffect,
        Situated,
        Draw,
        RecompositionReaction,
        HandleableEvent
      ](
       originalSituatedDraw =>drawModifier(shape, originalSituatedDraw.extract)
      )(
        updateDecorator[
          Update,
          PlacementEffect * Situated,
          Draw,
          RecompositionReaction,
          HandleableEvent
        ](
          old => (path, event) =>
            withClip(shape, old(path, event))
        )(PF.pure(placement))
      )
    )
  )
end clipWidget