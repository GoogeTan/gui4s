package gui4s.desktop.kit.cats
package widgets

import effects.*

import cats.*
import cats.data.*
import cats.effect.IO
import glfw4s.core.types.GlfwError
import gui4s.core.geometry.{Axis, InfinityOr, Point3d}
import gui4s.desktop.widget.library.{ContainerWidget, LinearContainer}

def container[
  Container[_] : Traverse,
  Event
](
  updateListOrdered : [A : Order, B] => (list: Container[A]) => (f: Container[A] => Update[Event, Container[B]]) => Update[Event, Container[B]]
) : ContainerWidget[DesktopPlacedWidget[Event], Container, Place, Point3d[Float]] =
  gui4s.desktop.kit.common.widgets.container[EitherT[IO, GlfwError, *], Container, Event](updateListOrdered)
end container

def linearContainer[Event] : LinearContainer[DesktopWidget[Event], OuterPlace, List, InfinityOr[Float], Float, Axis] =
  gui4s.desktop.kit.common.widgets.linearContainer[EitherT[IO, GlfwError, *], Event]
end linearContainer
