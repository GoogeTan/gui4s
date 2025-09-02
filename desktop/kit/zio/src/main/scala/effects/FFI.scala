package gui4s.desktop.kit.zio
package effects

import catnip.zio.ZioForeignFunctionInterface
import catnip.ForeignFunctionInterface
import zio.Task

given ffi: ForeignFunctionInterface[Task] = ZioForeignFunctionInterface()