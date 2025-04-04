package me.katze.gui4s.draw.skija

import cats.effect.IO
import me.katze.gui4s.impure.Impure
import org.lwjgl.glfw.GLFW.{GLFW_FALSE, GLFW_RESIZABLE, GLFW_TRUE, GLFW_VISIBLE, glfwDefaultWindowHints, glfwWindowHint}

final case class Window(handle : Long, enableVSync : Boolean, resizable : Boolean, width : Int, height : Int, dpi : Float, mouseX : Int, mouseY : Int)

object Window:
  def create[F[_] : Impure as I](title : String, width : Int, height : Int, resizable : Boolean) : F[Window] =
    I.impure:
      glfwDefaultWindowHints() // optional, the current window hints are already the default
      glfwWindowHint(GLFW_VISIBLE, GLFW_FALSE) // the window will stay hidden after creation
      glfwWindowHint(GLFW_RESIZABLE, GLFW_TRUE) // the window will be resizable

      ???
  end create
end Window
