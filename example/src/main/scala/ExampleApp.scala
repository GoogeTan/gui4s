package me.katze.gui4s.example

import draw.*
import update.ApplicationRequest

import cats.*
import cats.effect.*

object ExampleApp extends SwingApp:
  override def app(api: Library): api.Widget[ApplicationRequest] =
    api.label("12345")
  end app

  override type Library = SimpleDrawApiLibrary[IO, Int]
  
  override def createLibrary(drawApi: SimpleDrawApi[IO]) = SimpleDrawApiLibrary(drawApi)
end ExampleApp


