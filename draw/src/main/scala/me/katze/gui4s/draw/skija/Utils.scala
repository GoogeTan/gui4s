package me.katze.gui4s.draw.skija

import org.lwjgl.glfw.GLFW.{GLFW_FALSE, GLFW_TRUE}

extension (value : Boolean)
  def asGLFWBool: Int =
    if value then
      GLFW_TRUE
    else
      GLFW_FALSE
    end if
  end asGLFWBool
end extension
