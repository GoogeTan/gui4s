package me.katze.gui4s.example

import api.*
import update.ApplicationRequest

object ExampleApp extends Gui4sApp[Float]:
  override def rootWidget(using api : HighLevelApi & LabelApi[Unit] & LayoutApi[Float]): api.Widget[ApplicationRequest] =
    api.column(
      List(
        api.label("12345", ()),
        api.label("12345", ()),
        api.label("12345", ())
      ),
      MainAxisPlacementStrategy.SpaceBetween,
      AdditionalAxisPlacementStrategy.Begin
    )
  end rootWidget
end ExampleApp

