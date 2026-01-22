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
  PlacementEffect[_] : Functor as PF,
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
  freeWidgetToClip =>
    PF.map(
      freeWidgetToClip
    )(
      _.coflatMap { sizedWidget =>
          final case class ClipWidget(currentWidget: Widget[Update, PlacementEffect * Situated, Draw, RecompositionReaction, HandleableEvent], shape: Shape):
            def this(placed : Situated[Widget[Update, PlacementEffect * Situated, Draw, RecompositionReaction, HandleableEvent]]) =
              this(placed.extract, shapeFabric(placed.as(())))
            end this
          end ClipWidget
          
          given Functor[PlacementEffect * Situated] = nestedFunctorsAreFunctors[PlacementEffect, Situated]

          Widget.ValueWrapper[
            ClipWidget,
            Update,
            PlacementEffect * Situated,
            Draw,
            RecompositionReaction,
            HandleableEvent
          ](
            valueToDecorate = new ClipWidget(sizedWidget),
            valueAsFree = self => PF.map(self.currentWidget.asFree)(_.coflatMap(new ClipWidget(_))),
            valueIsDrawable = self => drawModifier(self.shape, self.currentWidget.draw),
            valueHandlesEvent = (self, path, event) =>
              withClip[PlacementEffect[Situated[Widget[Update, PlacementEffect * Situated, Draw, RecompositionReaction, HandleableEvent]]]](self.shape, self.currentWidget.handleEvent(path, event))
                .map(_.map(_.coflatMap(new ClipWidget(_)))),
            valueMergesWithOldState = (self, path, states) =>
              PF.map(self.currentWidget.mergeWithOldState(path, states))(_.coflatMap(new ClipWidget(_))),
            valueReactsOnRecomposition = (self, path, states) =>
              self.currentWidget.reactOnRecomposition(path, states),
            valueHasInnerState =
              self => self.currentWidget.innerStates
          )
      }
    )
end clipWidget