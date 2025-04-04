package me.katze.gui4s.example

import api.*
import update.ApplicationRequest

object ExampleApp extends SwingGui4sApp: 
  override def rootWidget[T <: HighLevelApi & LabelApi[Unit] & LayoutApi[Float]](using api : T): api.Widget[ApplicationRequest] =
    api.column(
      List(
        api.label("12345", ()),
        api.label("12345", ()),
        api.label("12345", ()),
        api.label("12345", ()),
        api.label("12345", ()),
        api.label("12345", ())
      ),
      MainAxisPlacementStrategy.SpaceBetween,
      AdditionalAxisPlacementStrategy.Begin
    )
  end rootWidget
end ExampleApp

