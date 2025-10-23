package gui4s.desktop.kit.zio
package widgets

import effects.*

import cats.*
import gui4s.core.geometry.{Axis, InfinityOr, Point3d}
import gui4s.desktop.widget.library.{ContainerWidget, LinearContainer}
import zio.*
import zio.interop.catz.*

def container[
  Container[_] : Traverse,
  Event
](
  updateListOrdered : [A : Order, B] => (list: Container[A]) => (f: Container[A] => Update[Event, Container[B]]) => Update[Event, Container[B]]
) : ContainerWidget[DesktopPlacedWidget[Event], Container, Place, Point3d[Float]] =
  gui4s.desktop.kit.common.widgets.container(updateListOrdered)
end container

def linearContainer[Event] : LinearContainer[DesktopWidget[Event], OuterPlace, List, InfinityOr[Float], Float, Axis] =
  gui4s.desktop.kit.common.widgets.linearContainer[Task, Event]
end linearContainer
