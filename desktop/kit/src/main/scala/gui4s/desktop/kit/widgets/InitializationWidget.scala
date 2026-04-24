package gui4s.desktop.kit.widgets

import scala.reflect.Typeable

import cats.effect.*

import gui4s.core.widget.library.WithContext


trait InitializationWidget:
  final def apply[
    Value: Typeable,
    Event,
  ](
    name : String,
    effectToRun : IO[Value],
    body : Value => DesktopWidget[Event],
    placeholder : DesktopWidget[Event],
  ) : DesktopWidget[Event] =
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
                                   ) : WithContext[DesktopWidget[Event], Option[Value]]
end InitializationWidget

object InitializationWidget:
  def apply(
    resourceWidget: ResourceWidget
  ) : InitializationWidget =
    new InitializationWidget:
      override def apply[Value: Typeable, Event](name: String, init: IO[Value]): WithContext[DesktopWidget[Event], Option[Value]] =
        resourceWidget(name, Resource.eval(init))
      end apply
    end new
  end apply
end InitializationWidget
