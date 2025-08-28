package gui4s.glfw

import cats.*
import org.lwjgl.glfw.GLFW.*

enum KeyAction:
  case Press, Release, Repeat
end KeyAction

object KeyAction:
  def fromCode(code : Int) : Option[KeyAction] =
    code match
      case GLFW_PRESS => Some(KeyAction.Press)
      case GLFW_REPEAT => Some(KeyAction.Repeat)
      case GLFW_RELEASE => Some(KeyAction.Release)
      case _ => None
    end match
  end fromCode

  given Eq[KeyAction] = {
    case (KeyAction.Press, KeyAction.Press) => true
    case (KeyAction.Release, KeyAction.Release) => true
    case (KeyAction.Repeat, KeyAction.Repeat) => true
    case _ => false
  }
end KeyAction
