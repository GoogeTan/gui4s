package me.katze.gui4s.glfw

import org.lwjgl.glfw.GLFW.*

enum KeyAction:
  case Press, Release, Repeat
end KeyAction

object KeyAction:
  def fromCode(code : Int) : KeyAction =
    code match
      case GLFW_PRESS => KeyAction.Press
      case GLFW_REPEAT => KeyAction.Repeat
      case GLFW_RELEASE => KeyAction.Release
      case _ => throw new Exception(s"Impossible key code: $code")
    end match
  end fromCode
end KeyAction
