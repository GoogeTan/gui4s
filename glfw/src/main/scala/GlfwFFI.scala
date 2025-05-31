package me.katze.gui4s.glfw

import cats.Monad
import cats.data.EitherT
import cats.syntax.all.*
import org.lwjgl.glfw.GLFW.{GLFW_NO_ERROR, glfwGetError}
import org.lwjgl.system.MemoryUtil

final class GlfwFFI[F[_] : {Monad, FFI as I}] extends FFI[EitherT[F, String, *]]:
  override def delay[A](trunk: => A): EitherT[F, String, A] =
    EitherT.liftF(I(trunk)) <* getError
  end delay

  override def blocking[A](trunk: => A): EitherT[F, String, A] =
    EitherT.liftF(I.blocking(trunk)) <* getError
  end blocking

  override def interruptible[A](trunk: => A): EitherT[F, String, A] =
    EitherT.liftF(I.interruptible(trunk)) <* getError
  end interruptible

  override def interruptibleMany[A](trunk: => A): EitherT[F, String, A] =
    EitherT.liftF(I.interruptibleMany(trunk)) <* getError
  end interruptibleMany

  def getError: EitherT[F, String, Unit] =
    EitherT(
      I.delay:
        val errorDesc = MemoryUtil.memAllocPointer(8)
        val errorCode = glfwGetError(errorDesc)
        if errorCode != GLFW_NO_ERROR then
          val errorText = errorDesc.getStringUTF8
          MemoryUtil.memFree(errorDesc)
          Left(errorText)
        else
          MemoryUtil.memFree(errorDesc)
          Right(())
    )
  end getError
end GlfwFFI
