package me.katze.gui4s.widget.library

import cats.{Applicative, Functor, Monoid}
import cats.syntax.all.*
import me.katze.gui4s.widget.draw.resourceIsDrawable
import me.katze.gui4s.widget.free.{AsFreeF, resourceAsFree}
import me.katze.gui4s.widget.handle.resourceHandlesEvent
import me.katze.gui4s.widget.{CatchEvents, Resource}
import me.katze.gui4s.widget.merge.{Mergable, resourceMergesWithOldState}
import me.katze.gui4s.widget.recomposition.resourceReactsOnRecomposition
import me.katze.gui4s.widget.state.resourceHasInnerStates

import scala.language.experimental.namedTypeArguments

type ResourceWidget[Widget, F[_]] = [T] => (name : String, resource : F[(T, F[Unit])]) => WithContext[Widget, Option[T]]

def resource[
  Update[_] : Applicative,
  Place[_] : Functor,
  F[_],
  Draw,
  RecompositionReaction : Monoid as M,
  HandlableEvent,
](
  fAsAction : F[Unit] => RecompositionReaction,
  widgetsAreMergable : Mergable[Place[Widget[Update, Place, Draw, RecompositionReaction, HandlableEvent]]]
) : ResourceWidget[
  Place[Widget[Update, Place, Draw, RecompositionReaction, HandlableEvent]],
  F
] =
  [T] => (name : String, allocator : F[(T, F[Unit])]) => freeWidget =>
    freeWidget(None).map:
      widget =>
        type Widget_ = Widget[Update, Place, Draw, RecompositionReaction, HandlableEvent]
        type ResourceState = Resource[Widget_, Option[(T, F[Unit])]]

        val resourceAsFree_ : AsFreeF[ResourceState, Place] = resourceAsFree(widgetAsFree)
        Widget.ValueWrapper[
          ResourceState,
          Update,
          Place,
          Draw,
          RecompositionReaction,
          HandlableEvent
        ](
          valueToDecorate = Resource(name, widget, None),
          valueAsFree = resourceAsFree_,
          valueIsDrawable = resourceIsDrawable(widgetIsDrawable),
          valueHandlesEvent = resourceHandlesEvent(
            widgetHandlesEvent = widgetHandlesEvent,
            resourceAsFree = resourceAsFree_,
            differentiateEvent = ???
          ),
          valueMergesWithOldState = resourceMergesWithOldState(
            widgetMergesWithOldState = widgetMergesWithOldState,
            resourceAsFree = resourceAsFree_,
            typeCheckState = ???
          ),
          valueReactsOnRecomposition = resourceReactsOnRecomposition(
            startResourceAllocation = ???,
            widgetReactsOnRecomposition = widgetReactsOnRecomposition
          ),
          valueHasInnerState = resourceHasInnerStates(
            hasInnerStates = widgetHasInnerStates,
            dealloc = _.map((value, destructor) => fAsAction(destructor)).getOrElse(M.empty)
          ),
        )
end resource
