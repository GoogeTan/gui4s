package gui4s.core.widget
package merge

trait UpdateWidgetStateFromTheOldOne[Place[_], Widget]:
  def mergeUpdatedAndRerenderedWidgets(
    pathToWidget : Path, 
    oldWidget: Place[Widget], 
    newWidget: Place[Widget]
  ): Place[Widget]


  def mergeOldAndRerenderedWidgets(
    pathToWidget : Path, 
    oldWidget: Widget, 
    newWidget: Place[Widget]
  ): Place[Widget]
end UpdateWidgetStateFromTheOldOne

