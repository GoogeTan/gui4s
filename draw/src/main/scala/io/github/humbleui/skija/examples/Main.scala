package io.github.humbleui.skija.examples

import cats.effect.{ExitCode, IO, IOApp}
import io.github.humbleui.types.*
import org.lwjgl.glfw.*
import org.lwjgl.glfw.GLFW.*

object Main extends IOApp:
  override def run(args: List[String]): IO[ExitCode] =
    (
    IO:
      GLFWErrorCallback.createPrint(System.err).set
      if !glfwInit then
        throw new IllegalStateException("Unable to initialize GLFW")
      end if

      val vidmode = glfwGetVideoMode(glfwGetPrimaryMonitor)
      val width = (vidmode.width * 0.75).toInt
      val height = (vidmode.height * 0.75).toInt
      val bounds = IRect.makeXYWH(Math.max(0, (vidmode.width - width) / 2), Math.max(0, (vidmode.height - height) / 2), width, height)
      new Window().run(bounds)
      ExitCode.Success
      ).evalOn(MainThread)
end Main

