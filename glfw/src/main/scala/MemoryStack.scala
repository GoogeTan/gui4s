package me.katze.gui4s.glfw

import catnip.ForeighFunctionInterface
import cats.effect.{Resource, Sync}
import org.lwjgl.system.MemoryStack

def stackPush[F[_] : Sync](ffi : ForeighFunctionInterface[F]): Resource[F, MemoryStack] =
  Resource.fromAutoCloseable(ffi.delay(MemoryStack.stackPush()))
end stackPush
