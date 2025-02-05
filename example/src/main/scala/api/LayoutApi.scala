package me.katze.gui4s.example
package api

trait LayoutApi[-MeasurementUnit] extends HighLevelApi:
  def row[Event](
                  children : List[Widget[Event]],
                  horizontalStrategy : MainAxisPlacementStrategy[MeasurementUnit],
                  verticalStrategy : AdditionalAxisPlacementStrategy
                ) : Widget[Event]
  def column[Event](
                     children : List[Widget[Event]],
                     verticalStrategy : MainAxisPlacementStrategy[MeasurementUnit],
                     horizontalStrategy : AdditionalAxisPlacementStrategy
                    ) : Widget[Event]
end LayoutApi

