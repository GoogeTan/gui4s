package gui4s.core.widget
package merge

trait UpdateWidgetStateFromTheOldOne[Widget]:
  def updateState(pathToWidget : Path, oldWidget: Widget, newWidget: Widget): Widget
end UpdateWidgetStateFromTheOldOne

