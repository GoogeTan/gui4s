package gui4s.desktop.kit.cats
package widgets

import effects.*

import cats.*
import cats.effect.IO
import cats.effect.std.Supervisor
import cats.syntax.all.*
import gui4s.desktop.kit.common.widgets.{initWidget, resource, resourceInit}
import gui4s.desktop.widget.library.{ResourceWidget, WithContext}

import scala.reflect.Typeable

@SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
given Typeable[IO[Unit]] = (x : Any) => x match {
  case a : IO[t] => Some(a.asInstanceOf[x.type & IO[Unit]])
  case _ => None
}

def resource[Event](
                      supervisor : Supervisor[IO],
                      raiseExternalEvent : DownEvent => IO[Unit]
                    ) : ResourceWidget[DesktopWidget[Event], IO] =
  gui4s.desktop.kit.common.widgets.resource[IO, Event](supervisor, raiseExternalEvent)
end resource

def resourceInit[Event, Value : Typeable](
  supervisor : Supervisor[IO],
  raiseExternalEvent : DownEvent => IO[Unit],
)(
  name : String,
  init : IO[Value]
) : WithContext[DesktopWidget[Event], Option[Value]] =
  gui4s.desktop.kit.common.widgets.resourceInit[IO, Event, Value](supervisor, raiseExternalEvent)(name, init)
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
  gui4s.desktop.kit.common.widgets.initWidget(supervisor, raiseExternalEvent)(name, imageSource, imageWidget, placeholder)
end initWidget
