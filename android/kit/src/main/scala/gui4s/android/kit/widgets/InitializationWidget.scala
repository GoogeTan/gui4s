package gui4s.android.kit.widgets

import cats.*
import cats.effect.Resource
import gui4s.core.widget.library.WithContext

import scala.reflect.Typeable


import cats.effect.IO
import cats.*
import cats.effect.Resource
import gui4s.core.widget.library.WithContext

import scala.reflect.Typeable


trait InitializationWidget:
  final def apply[
    Value: Typeable,
    Event,
  ](
    name : String,
    effectToRun : IO[Value],
    body : Value => AndroidWidget[Event],
    placeholder : AndroidWidget[Event],
  ) : AndroidWidget[Event] =
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
                                   ) : WithContext[AndroidWidget[Event], Option[Value]]
end InitializationWidget

object InitializationWidget:
  /**
   * TODO заменить ResourceWidget на [[gui4s.core.widget.library.initializeResourceWidget]]
   */
  def apply(
    resourceWidget: ResourceWidget
  ) : InitializationWidget =
    new InitializationWidget:
      override def apply[Value: Typeable, Event](name: String, init: IO[Value]): WithContext[AndroidWidget[Event], Option[Value]] =
        resourceWidget(name, Resource.eval(init))
      end apply
    end new
  end apply
end InitializationWidget
