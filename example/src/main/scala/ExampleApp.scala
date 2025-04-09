package me.katze.gui4s.example

import api.*
import update.ApplicationRequest

object ExampleApp extends SwingGui4sApp: 
  override def rootWidget[T <: HighLevelApi & TextWidgetApi[Unit] & LayoutApi[Float]](using api : T): api.Widget[ApplicationRequest] =
    api.column(
      List(
        api.text("12345", ()),
        api.text("12345", ()),
        api.text("12345", ()),
        api.text("12345", ()),
        api.text("12345", ()),
        api.text("12345", ())
      ),
      MainAxisPlacementStrategy.SpaceBetween,
      AdditionalAxisPlacementStrategy.Begin
    )
  end rootWidget
end ExampleApp

