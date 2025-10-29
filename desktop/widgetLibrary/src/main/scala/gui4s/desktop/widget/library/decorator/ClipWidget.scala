package gui4s.desktop.widget.library
package decorator

import catnip.syntax.all.{*, given}
import cats.syntax.all.*
import cats.{Comonad, Functor, Monad}

def clipWidget[
  Update[_] : Monad as UM,
  OuterPlace[_] : Functor as PF,
  InnerPlace[_] : Comonad,
  Draw,
  RecompositionReaction,
  HandleableEvent,
  Shape,
](
    withClip : [T] => (Shape, Update[T]) => Update[T],
    drawModifier : (Shape, Draw) => Draw,
    shapeFabric : InnerPlace[Unit] => Shape
) : Decorator[OuterPlace[InnerPlace[Widget[Update, OuterPlace * InnerPlace, Draw, RecompositionReaction, HandleableEvent]]]] =
  freeWidgetToClip =>
    PF.map(
      freeWidgetToClip
    )(
      _.coflatMap { sizedWidget =>
          final case class ClipWidget(currentWidget: Widget[Update, OuterPlace * InnerPlace, Draw, RecompositionReaction, HandleableEvent], shape: Shape):
            def this(placed : InnerPlace[Widget[Update, OuterPlace * InnerPlace, Draw, RecompositionReaction, HandleableEvent]]) =
              this(placed.extract, shapeFabric(placed.as(())))
            end this
          end ClipWidget
          
          given Functor[OuterPlace * InnerPlace] = nestedFunctorsAreFunctors[OuterPlace, InnerPlace]

          Widget.ValueWrapper[
            ClipWidget,
            Update,
            OuterPlace * InnerPlace,
            Draw,
            RecompositionReaction,
            HandleableEvent
          ](
            valueToDecorate = new ClipWidget(sizedWidget),
            valueAsFree = self => PF.map(self.currentWidget.asFree)(_.coflatMap(new ClipWidget(_))),
            valueIsDrawable = self => drawModifier(self.shape, self.currentWidget.draw),
            valueHandlesEvent = (self, path, event) =>
              withClip[OuterPlace[InnerPlace[Widget[Update, OuterPlace * InnerPlace, Draw, RecompositionReaction, HandleableEvent]]]](self.shape, self.currentWidget.handleEvent(path, event))
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