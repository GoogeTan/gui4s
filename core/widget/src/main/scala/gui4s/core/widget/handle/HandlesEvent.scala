package gui4s.core.widget
package handle

import cats.Functor
import cats.arrow.Arrow
import cats.arrow.Strong
import cats.syntax.all._

type HandlesEvent[-T, -EnvironmentalEvent, +UpdatedWidget] = (self : T, event : EnvironmentalEvent) => UpdatedWidget

type HandlesEvent_[-T, +UpdatedWidget] = (self : T) => UpdatedWidget
type HandlesEventF_[T, +F[_]] = T => F[T]

type HandlesEventF[T, -EnvironmentalEvent, +Update[_]] = HandlesEvent[T, EnvironmentalEvent, Update[T]]

given handlesEventIsArrow[Event] : Arrow[[A, B] =>> HandlesEvent[A, Event, B]] with
  override def compose[A, B, C](f: HandlesEvent[B, Event, C], g: HandlesEvent[A, Event, B]): HandlesEvent[A, Event, C] =
    (a, event) =>
      f(g(a, event), event)
  end compose

  override def lift[A, B](f: A => B): HandlesEvent[A, Event, B] =
    (a, _) => f(a)
  end lift

  override def first[A, B, C](fa: HandlesEvent[A, Event, B]): HandlesEvent[(A, C), Event, (B, C)] =
    (self, event) =>
      (fa(self._1, event), self._2)
  end first
end handlesEventIsArrow


given handlesEventFIsStrong[F[_] : Functor, Event]: Strong[[A, B] =>> HandlesEvent[A, Event, F[B]]] with
  override def dimap[A, B, C, D](fab: HandlesEvent[A, Event, F[B]])(f: C => A)(g: B => D): HandlesEvent[C, Event, F[D]] =
    (c, event) => fab(f(c), event).map(g)
  end dimap

  override def first[A, B, C](fa: HandlesEvent[A, Event, F[B]]): HandlesEvent[(A, C), Event, F[(B, C)]] =
    (self, event) =>
      fa(self._1, event).map(b => (b, self._2))
  end first

  override def second[A, B, C](fa: HandlesEvent[A, Event, F[B]]): HandlesEvent[(C, A), Event, F[(C, B)]] =
    (self, event) =>
      fa(self._2, event).map(b => (self._1, b))
  end second
end handlesEventFIsStrong

given handlesEventF_IsStrong[F[_] : Functor]: Strong[[A, B] =>> HandlesEvent_[A, F[B]]] with
  override def dimap[A, B, C, D](fab: HandlesEvent_[A, F[B]])(f: C => A)(g: B => D): HandlesEvent_[C, F[D]] =
    c => fab(f(c)).map(g)
  end dimap

  override def first[A, B, C](fa: HandlesEvent_[A, F[B]]): HandlesEvent_[(A, C), F[(B, C)]] =
    self =>
      fa(self._1).map(b => (b, self._2))
  end first

  override def second[A, B, C](fa: HandlesEvent_[A, F[B]]): HandlesEvent_[(C, A), F[(C, B)]] =
    self =>
      fa(self._2).map(b => (self._1, b))
  end second
end handlesEventF_IsStrong
