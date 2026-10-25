package gui4s.core.widget.library

final case class LayoutIncrementalWidget[Widget, Place[_], Meta](
  oldPlacedWidget : (Widget, Meta),
  newWidget : Option[Place[Widget]]
):
  def widget : Widget = oldPlacedWidget._1
  def meta : Meta = oldPlacedWidget._2
end LayoutIncrementalWidget

