package gui4s.core.widget
package merge

trait UpdateWidgetStateFromTheOldOne[Place[_], Widget]:
  def mergeUpdatedAndRerenderedWidgets(
    oldWidget: Place[Widget], 
    newWidget: Place[Widget]
  ): Place[Widget]


  def mergeOldAndRerenderedWidgets(
    oldWidget: Widget, 
    newWidget: Place[Widget]
  ): Place[Widget]
end UpdateWidgetStateFromTheOldOne

