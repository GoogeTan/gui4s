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
  Shape,
](
    withClip : [T] => (Shape, Update[T]) => Update[T],
    drawModifier : (Shape, Draw) => Draw,
    shapeFabric : Situated[WidgetWithSituated[Update, PlacementEffect, Situated, Draw, RecompositionReaction]] => PlacementEffect[Situated[Shape]]
) : Decorator[FreeWidgetWithSituated[Update, PlacementEffect, Situated, Draw, RecompositionReaction]] =
  given Functor[PlacementEffect * Situated] = nestedFunctorsAreFunctors[PlacementEffect, Situated]

  placementDecorator[
    Update,
    PlacementEffect * Situated,
    PlacementEffect * Situated,
    Draw,
    RecompositionReaction
  ]((test : PlacementEffect[Situated[WidgetWithSituated[Update, PlacementEffect, Situated, Draw, RecompositionReaction]]]) =>
    test.sigmaProduct(shapeFabric).flatMap((placement, shape) =>
      drawDecorator[
        Update,
        PlacementEffect,
        Situated,
        Draw,
        RecompositionReaction
      ](
       originalSituatedDraw =>drawModifier(shape.extract, originalSituatedDraw.extract)
      )(
        updateDecorator[
          Update,
          PlacementEffect * Situated,
          Draw,
          RecompositionReaction
        ](
          old => event =>
            withClip(shape.extract, old(event))
        )(PF.pure(shape.as(placement.extract)))
      )
    ),
    _
  )
end clipWidget