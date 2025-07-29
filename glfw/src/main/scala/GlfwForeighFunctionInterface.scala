package me.katze.gui4s.glfw

import catnip.ForeighFunctionInterface
import cats.MonadError
import cats.syntax.all.*
import org.lwjgl.glfw.GLFW.{GLFW_NO_ERROR, glfwGetError}
import org.lwjgl.system.MemoryUtil

final class GlfwForeighFunctionInterface[
  F[_] : ForeighFunctionInterface as I,
  Error
](makeError : (code : Int, text : String) => Error)(using M : MonadError[F, Error]) extends ForeighFunctionInterface[F]:
  override def delay[A](trunk: => A): F[A] =
    I(trunk) <* getError
  end delay

  override def blocking[A](trunk: => A): F[A] =
    I.blocking(trunk) <* getError
  end blocking

  override def interruptible[A](trunk: => A): F[A] =
    I.interruptible(trunk) <* getError
  end interruptible

  override def interruptibleMany[A](trunk: => A): F[A] =
    I.interruptibleMany(trunk) <* getError
  end interruptibleMany

  def getError: F[Unit] =
    (
      I.delay:
        val errorDesc = MemoryUtil.memAllocPointer(8)
        val errorCode = glfwGetError(errorDesc)
        if errorCode != GLFW_NO_ERROR then
          val errorText = errorDesc.getStringUTF8
          MemoryUtil.memFree(errorDesc)
          M.raiseError(makeError(errorCode, errorText))
        else
          MemoryUtil.memFree(errorDesc)
          M.pure(())
    ).flatten
  end getError
end GlfwForeighFunctionInterface
