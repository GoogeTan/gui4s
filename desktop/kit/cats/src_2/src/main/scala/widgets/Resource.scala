package gui4s.desktop.kit.cats
package widgets

import effects.*

import cats.*
import cats.data.*
import cats.effect.IO
import cats.effect.std.Supervisor
import glfw4s.core.types.GlfwError
import gui4s.desktop.widget.library.{ResourceWidget, WithContext}

import scala.reflect.Typeable

@SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
given Typeable[IO[Unit]] = (x : Any) => x match {
  case a : IO[t] => Some(a.asInstanceOf[x.type & IO[Unit]])
  case _ => None
}

def resource[Event](
                      supervisor : Supervisor[EitherT[IO, GlfwError, *]],
                      raiseExternalEvent : DownEvent => EitherT[IO, GlfwError, Unit]
                    ) : ResourceWidget[DesktopWidget[Event], IO] =
  gui4s.desktop.kit.common.widgets.resource[EitherT[IO, GlfwError, *], Event](supervisor, raiseExternalEvent)
end resource

def resourceInit[Event, Value : Typeable](
  supervisor : Supervisor[EitherT[IO, GlfwError, *]],
  raiseExternalEvent : DownEvent => EitherT[IO, GlfwError, Unit],
)(
  name : String,
  init : EitherT[IO, GlfwError, Value]
) : WithContext[DesktopWidget[Event], Option[Value]] =
  gui4s.desktop.kit.common.widgets.resourceInit[EitherT[IO, GlfwError, *], Event, Value](supervisor, raiseExternalEvent)(name, init)
end resourceInit

def initWidget[
  Event,
  Value : Typeable
](
    supervisor: Supervisor[EitherT[IO, GlfwError, *]],
    raiseExternalEvent : DownEvent => EitherT[IO, GlfwError, Unit],
  )(
    name : String,
    imageSource : EitherT[IO, GlfwError, Value],
    imageWidget : Value => DesktopWidget[Event],
    placeholder : DesktopWidget[Event],
  ) : DesktopWidget[Event] =
  gui4s.desktop.kit.common.widgets.initWidget[
    EitherT[IO, GlfwError, *],
    Event,
    Value
  ](
    supervisor, raiseExternalEvent
  )(name, imageSource, imageWidget, placeholder)
end initWidget
