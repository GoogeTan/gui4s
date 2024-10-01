package me.katze.gui4s.example

import api.{HighLevelApi, LabelApi, LayoutApi}
import update.ApplicationRequest

object ExampleApp extends Gui4sApp[Float]:
  override def app(using api : HighLevelApi & LabelApi[Unit] & LayoutApi[Float]): api.Widget[ApplicationRequest] =
    api.label("12345", ())
  end app
end ExampleApp

