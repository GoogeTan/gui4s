package gui4s.glfw

import catnip.ForeignFunctionInterface
import cats.effect.{Resource, Sync}
import org.lwjgl.system.MemoryStack

def stackPush[F[_] : Sync](ffi : ForeignFunctionInterface[F]): Resource[F, MemoryStack] =
  Resource.fromAutoCloseable(ffi.delay(MemoryStack.stackPush()))
end stackPush
