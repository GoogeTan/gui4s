package gui4s.desktop.kit.cats
package widgets

import effects.Draw.given
import effects.OuterPlace.given
import effects.Place.given
import effects.Update.given
import effects.{*, given}

import cats.*
import cats.syntax.all.*
import cats.effect.IO
import gui4s.core.geometry.{Axis, InfinityOr, Point3d}
import gui4s.desktop.widget.library.{ContainerWidget, LinearContainer}

def container[
  Container[_] : Traverse,
  Event
](
  updateListOrdered : [A : Order, B] => (list: Container[A]) => (f: Container[A] => Update[Event, Container[B]]) => Update[Event, Container[B]]
) : ContainerWidget[DesktopPlacedWidget[Event], Container, Place, Point3d[Float]] =
  gui4s.desktop.kit.common.widgets.container(updateListOrdered)
end container

def linearContainer[Event] : LinearContainer[DesktopWidget[Event], OuterPlace, List, InfinityOr[Float], Float, Axis] =
  gui4s.desktop.kit.common.widgets.linearContainer[IO, Event]
end linearContainer
