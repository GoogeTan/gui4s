package gui4s.desktop.kit.cats
package effects

import catnip.ForeignFunctionInterface
import catnip.effect.SyncForeignFunctionInterface
import cats.effect.IO

given ffi: ForeignFunctionInterface[IO] = SyncForeignFunctionInterface[IO]()