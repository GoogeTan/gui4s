package me.katze.gui4s.example
package api

import me.katze.gui4s.layout.Axis

trait LayoutApi[Widget[_], -MeasurementUnit]:
  final def row[Event](
                  children : List[Widget[Event]],
                  horizontalStrategy : MainAxisPlacementStrategy[MeasurementUnit],
                  verticalStrategy : AdditionalAxisPlacementStrategy
                ) : Widget[Event] =
    linearLayout(children, Axis.Horizontal, horizontalStrategy, verticalStrategy)
  end row

  final def column[Event](
                      children : List[Widget[Event]],
                      verticalStrategy : MainAxisPlacementStrategy[MeasurementUnit],
                      horizontalStrategy : AdditionalAxisPlacementStrategy
                    ) : Widget[Event] =
    linearLayout(children, Axis.Vertical, verticalStrategy, horizontalStrategy)
  end column

  def linearLayout[Event](
                           children: List[Widget[Event]],
                           axis: Axis,
                           mainAxisStrategy: MainAxisPlacementStrategy[MeasurementUnit],
                           additionalAxisStrategy: AdditionalAxisPlacementStrategy,
                         ): Widget[Event]
end LayoutApi

