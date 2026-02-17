package gui4s.desktop.kit.widgets

import catnip.syntax.all.given
import cats.effect.*
import gui4s.core.geometry.{Axis, InfinityOr}
import gui4s.core.widget.library.WeightedLinearContainer
import gui4s.desktop.kit.effects.*

def weightedLinearContainer[
  Event
] : WeightedLinearContainer[
  DesktopWidget[Event],
  PlacementEffectC[IO],
  List,
  InfinityOr[Float],
  Float,
  Axis
] =
  gui4s.core.widget.library.weightedLinearContainer(
    listContainerWidget,
    PlacementEffect.getBounds,
    PlacementEffect.setBounds,
    _.minus(_),
    (bounds, weight) => bounds * weight,
    _.value,
  )
end weightedLinearContainer
