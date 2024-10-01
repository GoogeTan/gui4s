package me.katze.gui4s.example
package api

import me.katze.gui4s.widget.stateful.{EventReaction, RichTypeChecker}

trait HighLevelApi:
  type Widget[+_]
  type WidgetTask[+_]
end HighLevelApi
