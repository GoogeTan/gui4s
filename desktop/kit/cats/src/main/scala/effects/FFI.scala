package gui4s.desktop.kit.cats
package effects

import catnip.ForeignFunctionInterface
import catnip.effect.SyncForeignFunctionInterface
import cats.effect.IO
import cats.syntax.all.*

given ffi: ForeignFunctionInterface[IO] = SyncForeignFunctionInterface[IO]()