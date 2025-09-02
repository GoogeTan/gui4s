package gui4s.desktop.kit.zio
package widgets

import effects.*

import cats.*
import cats.effect.std.Supervisor
import gui4s.desktop.kit.common.widgets.{initWidget, resource, resourceInit}
import gui4s.desktop.widget.library.{ResourceWidget, WithContext}
import zio.*
import zio.interop.catz.*

import scala.reflect.Typeable

def resource[Event](
                      supervisor : Supervisor[Task],
                      raiseExternalEvent : DownEvent => Task[Unit]
                    ) : ResourceWidget[DesktopWidget[Event], Task] =
  gui4s.desktop.kit.common.widgets.resource[Task, Event](supervisor, raiseExternalEvent)
end resource

def resourceInit[Event, Value : Typeable](
  supervisor : Supervisor[Task],
  raiseExternalEvent : DownEvent => Task[Unit],
)(
  name : String,
  init : Task[Value]
) : WithContext[DesktopWidget[Event], Option[Value]] =
  gui4s.desktop.kit.common.widgets.resourceInit[Task, Event, Value](supervisor, raiseExternalEvent)(name, init)
end resourceInit

def initWidget[
  Event,
  Value
](
    supervisor: Supervisor[Task],
    raiseExternalEvent : DownEvent => Task[Unit],
  )(
    name : String,
    imageSource : Task[Value],
    imageWidget : Value => DesktopWidget[Event],
    placeholder : DesktopWidget[Event],
  ) : DesktopWidget[Event] =
  gui4s.desktop.kit.common.widgets.initWidget(supervisor, raiseExternalEvent)(name, imageSource, imageWidget, placeholder)
end initWidget
