package me.katze.gui4s.widget
package library

trait DrawOnlyWidgetLibrary extends WidgetLibrary:
  private class DrawOnlyWidget(
                            override val asFree : FreeWidget[Nothing, Any],
                            override val draw : Draw
                          ) extends me.katze.gui4s.widget.PlacedWidget[Draw, WidgetTask[Any], FreeWidget, Nothing, Any]:
    override def handleDownEvent(event: Any): EventResult[WidgetTask[Any], FreeWidget[Nothing, Any], Nothing] = EventResult(asFree)
    override def mergeWithState(oldState: Map[String, Any]): FreeWidget[Nothing, Any] = asFree
    override def childrenStates: Map[String, Any] = Map()
  end DrawOnlyWidget

  final def drawOnlyWidget(asFree: FreeWidget[Nothing, Any], draw: Draw): PlacedWidget[Nothing, Any] =
    constructRealWidget(DrawOnlyWidget(asFree, draw))
  end drawOnlyWidget
end DrawOnlyWidgetLibrary
