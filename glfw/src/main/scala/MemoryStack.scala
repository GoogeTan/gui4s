package gui4s.glfw

import cats.effect.{Resource, Sync}
import org.lwjgl.system.MemoryStack

def stackPush[F[_] : Sync as S]: Resource[F, MemoryStack] =
  Resource.fromAutoCloseable(S.delay(MemoryStack.stackPush()))
end stackPush
