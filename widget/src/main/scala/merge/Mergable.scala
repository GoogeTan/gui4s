package me.katze.gui4s.widget
package merge

trait Mergable[Widget]:
  def merge(pathToWidget : Path, oldWidget: Widget, newWidget: Widget): Widget
end Mergable

