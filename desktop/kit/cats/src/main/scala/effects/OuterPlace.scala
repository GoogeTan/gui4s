package gui4s.desktop.kit.cats
package effects

import catnip.{Get, Set}
import cats.data.EitherT
import cats.effect.IO
import cats.*
import gui4s.core.kit.effects.{OuterPlace as GenericOuterPlace, OuterPlaceOps}

type OuterPlace[T] = GenericOuterPlace[IO, Bounds, String, T]

object OuterPlace extends OuterPlaceOps[IO, Bounds, String]
