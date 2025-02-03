package me.katze.gui4s.example
package api

trait LayoutApi[-MU] extends HighLevelApi:
  def row[Event](
                  children : List[Widget[Event]],
                  horizontalStrategy : MainAxisPlacementStrategy[MU],
                  verticalStrategy : AdditionalAxisPlacementStrategy
                ) : Widget[Event]
  def column[Event](
                     children : List[Widget[Event]],
                     verticalStrategy : MainAxisPlacementStrategy[MU],
                     horizontalStrategy : AdditionalAxisPlacementStrategy
                    ) : Widget[Event]
end LayoutApi

