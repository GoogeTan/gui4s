package gui4s.desktop.kit.widgets

import scala.reflect.Typeable

import cats._
import cats.effect.Resource

import gui4s.core.widget.library.WithContext


trait InitializationWidget[IO[_]]:
  final def apply[
    Value: Typeable,
    Event,
  ](
    name : String,
    effectToRun : IO[Value],
    body : Value => DesktopWidget[IO, Event],
    placeholder : DesktopWidget[IO, Event],
  ) : DesktopWidget[IO, Event] =
    apply[Value, Event](
      name,
      effectToRun
    ) {
      case Some(value) => body(value)
      case None => placeholder
    }
  end apply

  def apply[Value: Typeable, Event](
                                     name: String,
                                     init: IO[Value]
                                   ) : WithContext[DesktopWidget[IO, Event], Option[Value]]
end InitializationWidget

object InitializationWidget:
  def apply[
    IO[_] : Applicative,
  ](
    resourceWidget: ResourceWidget[IO]
  ) : InitializationWidget[IO] =
    new InitializationWidget[IO]:
      override def apply[Value: Typeable, Event](name: String, init: IO[Value]): WithContext[DesktopWidget[IO, Event], Option[Value]] =
        resourceWidget(name, Resource.eval(init))
      end apply
    end new
  end apply
end InitializationWidget
