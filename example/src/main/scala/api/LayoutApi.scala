package me.katze.gui4s.example
package api

import api.impl.*

trait LayoutApi[-MU] extends HighLevelApi:
  def row[Event](
                  children : List[Widget[Event]],
                  horizontalStrategy : MainAxisStrategy[MU],
                  verticalStrategy : AdditionalAxisStrategy
                ) : Widget[Event]
  def column[Event](
                      children : List[Widget[Event]],
                      verticalStrategy : MainAxisStrategy[MU],
                      horizontalStrategy : AdditionalAxisStrategy
                    ) : Widget[Event]
end LayoutApi

