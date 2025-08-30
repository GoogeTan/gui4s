package gui4s.desktop.kit
package effects

import catnip.{Get, Set}
import cats.data.EitherT
import cats.effect.IO
import cats.*
import gui4s.core.kit.effects.{OuterPlace as GenericOuterPlace, OuterPlaceOps}

type OuterPlace[IO[_], T] = GenericOuterPlace[IO, Bounds, Throwable, T]

trait OuterPlaceOps[IO[_]] extends OuterPlaceOps[IO, Bounds, Throwable]
