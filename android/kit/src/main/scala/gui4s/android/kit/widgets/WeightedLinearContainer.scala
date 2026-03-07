package gui4s.android.kit.widgets

import catnip.syntax.all.given
import cats.effect.*
import gui4s.android.kit.effects.*
import gui4s.core.geometry.{Axis, InfinityOr, Point3d, Rect}
import gui4s.core.layout.Weighted
import gui4s.core.widget.handle.LayoutIncrementalWidget
import gui4s.core.widget.library.{GenericLayout, LayersMetadata, WeightedLinearContainer}

def weightedLinearContainer[
  Event
] : WeightedLinearContainer[
  AndroidWidget[Event],
  PlacementEffect,
  List,
  InfinityOr[Float],
  Float,
  Axis
] =
  type WeightedMetadata = LayersMetadata[(Option[Float], Point3d[Float]), Rect[Float], Rect[InfinityOr[Float]]]
  gui4s.core.widget.library.weightedLinearContainer(
    (children, placement) =>
      containerWidget2[
        List,
        Event,
        Weighted[AndroidWidget[Event]],
        Weighted[LayoutIncrementalWidget[AndroidPlacedWidget[Event], Place, WeightedMetadata]],
        WeightedMetadata
      ](
        childrenIn = children,
        layout = GenericLayout(
          measureWithBounds = _.map(measureWithBounds),
          measureWithBoundsIncrementally = _.map(measureWithBoundsIncrementally),
          getBounds = PlacementEffect.getBounds,
          placementStrategy = placement
        ),
        incrementalFreeChildrenFromPlaced = { case ((widget, meta), maybeNew) =>
          Weighted(
            LayoutIncrementalWidget((widget, meta), maybeNew),
            meta.point._1
          )
        },
        pointOfMeta = _.point._2
      ),
    PlacementEffect.getBounds,
    PlacementEffect.setBounds,
    _.minus(_),
    (bounds, weight) => bounds * weight,
  )
end weightedLinearContainer
