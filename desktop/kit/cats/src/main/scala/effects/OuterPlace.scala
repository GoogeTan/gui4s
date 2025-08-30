package gui4s.desktop.kit.cats
package effects

import catnip.{Get, Set}
import cats.*
import cats.data.EitherT
import cats.effect.IO
import gui4s.core.kit.effects.{OuterPlaceOps, OuterPlace as GenericOuterPlace}

type OuterPlace[T] = GenericOuterPlace[IO, Bounds, String, T]

object OuterPlace extends OuterPlaceOps[IO, Bounds, String]
