package gui4s.desktop.widget.library

import scala.reflect.Typeable

import catnip.syntax.all.{_, given}
import cats._
import cats.arrow.Strong
import cats.syntax.all._

import gui4s.core.widget.LaunchedEffect
import gui4s.core.widget.Path
import gui4s.core.widget.handle.{_, given}
import gui4s.core.widget.merge.launchedEffectMergesWithOldState
import gui4s.core.widget.recomposition.{_, given}
import gui4s.core.widget.state.{_, given}

type LaunchedEffectWidget[Widget, Key, Task] = (name : String, child : Widget, key : Key, task : Task) => Widget

// TODO надо написать в доке, что он не гарантиурет остановку ИО при смене ключа
// TODO добавить тесты на добавление имен в Place
def launchedEffect[
  Update[_] : Applicative as UA,
  Place[_] : Functor as PF,
  Draw,
  RecompositionReaction : Monoid as M,
  Key : {Typeable as KT, Equiv}
](
  keyTypeError : [T] => (Path, Any) => Place[T],
  keysTypeMismatchError : Any => RecompositionReaction,
  addNameToPath : String => Place ~> Place
) : LaunchedEffectWidget[Place[Widget[Update, Place, Draw, RecompositionReaction]], Key, Path => RecompositionReaction] =
  type PlacedWidget = Widget[Update, Place, Draw, RecompositionReaction]
  given updateOptionFunctor : Functor[Update * Option] = nestedFunctorsAreFunctors[Update, Option]()
  given updateOptionPlaceFunctor : Functor[Update * Option * Place] = nestedFunctorsAreFunctors[Update * Option, Place]()
  (name, freeChild, key, task) =>
    addNameToPath(name)(freeChild).map:
      child =>
        Widget.ValueWrapper[
          (
            LaunchedEffect[
              Key, 
              Path => RecompositionReaction,
            ],
            PlacedWidget
          ),
          Update, 
          Place, 
          Draw, 
          RecompositionReaction
        ](
          valueToDecorate = (LaunchedEffect(name, key, task), child),
          valueAsFree = Strong[[A, B] =>> A => Place[B]].second(widgetAsFree),
          valueIsDrawable = Contravariant[* => Draw].contramap(widgetIsDrawable)(_._2),
          valueHandlesEvent =
            handlesEventF_IsStrong[Update * Option * Place]
              .second(
                mapEventHandle_(
                  widgetHandlesEvent
                )(_.map(_.map(addNameToPath(name)(_))))
              ),
          valueMergesWithOldState = launchedEffectMergesWithOldState(using KT, PF)(
            keyTypeError, 
            widgetMergesWithOldState,
            widgetAsFree
          ),
          valueReactsOnRecomposition = reactsOnRecompositionIsContravariantMonoidal.product(
            launchedEffectReactsOnRecomposition(M.empty, keysTypeMismatchError),
            widgetReactsOnRecomposition
          ),
          valueHasInnerState = hasInnerStatesIsContravariantMonoidal.product(launchedEffectHasInnerState(M.empty), widgetHasInnerStates),
        )