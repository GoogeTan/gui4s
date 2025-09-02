package gui4s.desktop.kit.cats
package widgets

import effects.*
import effects.Place.given
import effects.Update.given

import cats.effect.IO

import cats.*
import cats.effect.std.Supervisor
import cats.syntax.all.*
import gui4s.core.widget.Path
import gui4s.desktop.widget.library.{ResourceWidget, WithContext}

import scala.reflect.Typeable

def resource[Event](
                      supervisor : Supervisor[IO],
                      raiseExternalEvent : DownEvent => IO[Unit]
                    ) : ResourceWidget[DesktopWidget[Event], IO] =
  gui4s.desktop.kit.widgets.resource[IO, Event](supervisor, raiseExternalEvent)
end resource

def resourceInit[Event, Value : Typeable](
  supervisor : Supervisor[IO],
  raiseExternalEvent : DownEvent => IO[Unit],
)(
  name : String,
  init : IO[Value]
) : WithContext[DesktopWidget[Event], Option[Value]] =
  gui4s.desktop.kit.widgets.resourceInit[IO, Event, Value](supervisor, raiseExternalEvent)(name, init)
end resourceInit

def initWidget[
  Event,
  Value
](
    supervisor: Supervisor[IO],
    raiseExternalEvent : DownEvent => IO[Unit],
  )(
    name : String,
    imageSource : IO[Value],
    imageWidget : Value => DesktopWidget[Event],
    placeholder : DesktopWidget[Event],
  ) : DesktopWidget[Event] =
  gui4s.desktop.kit.widgets.initWidget(supervisor, raiseExternalEvent)(name, imageSource, imageWidget, placeholder)
end initWidget
