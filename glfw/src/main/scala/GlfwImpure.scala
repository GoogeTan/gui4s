package me.katze.gui4s.glfw

import cats.Monad
import cats.data.EitherT
import cats.syntax.all.*
import me.katze.gui4s.impure.{Impure, ImpureError}
import org.lwjgl.glfw.GLFW.{GLFW_NO_ERROR, glfwGetError}
import org.lwjgl.system.MemoryUtil

final class GlfwImpure[F[_] : {Monad, Impure as I}] extends Impure[[Value] =>> EitherT[F, String, Value]] with ImpureError[[Value] =>> EitherT[F, String, Value], String]:
  override def impure[A](trunk: => A): EitherT[F, String, A] =
    EitherT.liftF(I(trunk)) <* getError
  end impure

  override def impureTry[Value](from: => Either[String, Value]): EitherT[F, String, Value] =
    EitherT(I.impure(from)) <* getError
  end impureTry

  def getError: EitherT[F, String, Unit] =
    EitherT(
      I.impure:
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
end GlfwImpure
