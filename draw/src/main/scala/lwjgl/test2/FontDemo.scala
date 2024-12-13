package me.katze.gui4s.draw
package lwjgl.test2

import lwjgl.GLFWUtil.glfwInvoke

import org.lwjgl.glfw.Callbacks.glfwFreeCallbacks
import org.lwjgl.glfw.GLFW.*
import org.lwjgl.glfw.GLFWErrorCallback
import org.lwjgl.opengl.GL11.*
import org.lwjgl.opengl.{GL, GLUtil}
import org.lwjgl.system.MemoryStack.stackPush
import org.lwjgl.system.MemoryUtil.NULL
import org.lwjgl.system.{Callback, MemoryStack, Platform}

import java.lang.Math.{max, min}
import java.util.Objects
import java.util.regex.Pattern
import scala.util.{Try, Using}

final case class OGLWindow(
                            var window: Long,
                            var ww: Int,
                            var wh: Int,
                            var debugProc: Callback | Null,
                          )

final case class Line(
                        text : String,
                        lineCount: Int,
                        fontHeight: Int,
                        var lineHeight: Float,
                        var kerningEnabled: Boolean,
                        var lineBBEnabled: Boolean,
                      )

final case class State(
                        var scale: Int,
                        var contentScaleX: Float,
                        var contentScaleY: Float,
                        var ctrlDown: Boolean,
                        var lineOffset: Int,
                      )


final case class FontDemo(
                            window : OGLWindow,
                            line : Line,
                            state : State
                          ):
  def setLineOffset(offset: Float): Unit =
    setLineOffset(offset.round)

  def setLineOffset(offset: Int): Unit =
    state.lineOffset = max(0, min(offset, line.lineCount - (window.wh / line.lineHeight).toInt))

  def setScale(scale: Int): Unit =
    state.scale = max(-3, scale)
    line.lineHeight = line.fontHeight * (1.0f + state.scale * 0.25f)
    setLineOffset(state.lineOffset)

  def destroy(): Unit =
    GL.setCapabilities(null)
    if (window.debugProc != null)
      window.debugProc.free()
    glfwFreeCallbacks(window.window)
    glfwDestroyWindow(window.window)
    glfwTerminate()
    Objects.requireNonNull(glfwSetErrorCallback(null)).free()
  end destroy
end FontDemo

object FontDemo:
  def linesOfText(text : String) : Int =
    var lc = 0
    val m = Pattern.compile("^.*$", Pattern.MULTILINE).matcher(text)
    while m.find do
      lc += 1
    end while
    lc
  end linesOfText

  @SuppressWarnings(Array("org.wartremover.warts.All"))
  def apply(text : String, fontHeight : Int) : FontDemo =
    new FontDemo(
      OGLWindow(0, 800, 600, null),
      Line(text, linesOfText(text), fontHeight, 0, true, false),
      State(0, 0, 0, false, 0)
    )
  end apply
end FontDemo

def initFrameBuffer(monitor : Long, self : FontDemo) : Try[(Int, Int)] =
  Using(stackPush):
    s =>
      val px = s.mallocFloat(1)
      val py = s.mallocFloat(1)
      glfwGetMonitorContentScale(monitor, px, py)
      self.state.contentScaleX = px.get(0)
      self.state.contentScaleY = py.get(0)
      if Platform.get eq Platform.MACOSX then
        (self.window.ww, self.window.wh)
      else
        (self.window.ww * self.state.contentScaleX.round, self.window.wh * self.state.contentScaleY.round)
end initFrameBuffer


def initFD(self: FontDemo, title: String): Unit =
  GLFWErrorCallback.createPrint.set
  if (!glfwInit)
    throw new IllegalStateException("Unable to initialize GLFW")
  glfwDefaultWindowHints()
  glfwWindowHint(GLFW_VISIBLE, GLFW_FALSE)
  glfwWindowHint(GLFW_RESIZABLE, GLFW_TRUE)
  glfwWindowHint(GLFW_OPENGL_DEBUG_CONTEXT, GLFW_TRUE)
  val monitor = glfwGetPrimaryMonitor
  val (framebufferW, framebufferH) = initFrameBuffer(monitor, self).get
  self.window.window = glfwCreateWindow(framebufferW, framebufferH, title, NULL, NULL)
  if (self.window.window == NULL)
    throw new RuntimeException("Failed to create the GLFW window")
  registerCallbacks(self)
  // Center window
  val vidmode = Objects.requireNonNull(glfwGetVideoMode(monitor))
  glfwSetWindowPos(self.window.window, (vidmode.width - framebufferW) / 2, (vidmode.height - framebufferH) / 2)
  // Create context
  glfwMakeContextCurrent(self.window.window)
  GL.createCapabilities
  self.window.debugProc = GLUtil.setupDebugMessageCallback
  glfwSwapInterval(1)
  glfwShowWindow(self.window.window)
  glfwInvoke(self.window.window, windowSizeChanged2(self, _, _, _), framebufferSizeChanged)
end initFD

def registerCallbacks(self : FontDemo) : Unit =
  glfwSetWindowSizeCallback(self.window.window, windowSizeChanged2(self, _, _, _))
  glfwSetFramebufferSizeCallback(self.window.window, framebufferSizeChanged)
  glfwSetKeyCallback(self.window.window, (window: Long, key: Int, _: Int, action: Int, _: Int) =>
    key match
      case GLFW_KEY_LEFT_CONTROL =>
      case GLFW_KEY_RIGHT_CONTROL =>
        self.state.ctrlDown = action != GLFW_RELEASE
    end match

    if action != GLFW_RELEASE then
      key match
        case GLFW_KEY_ESCAPE =>
          glfwSetWindowShouldClose(window, true)
        case GLFW_KEY_PAGE_UP =>
          self.setLineOffset(self.state.lineOffset - self.window.wh / self.line.lineHeight)
        case GLFW_KEY_PAGE_DOWN =>
          self.setLineOffset(self.state.lineOffset + self.window.wh / self.line.lineHeight)
        case GLFW_KEY_HOME =>
          self.setLineOffset(0)
        case GLFW_KEY_END =>
          self.setLineOffset(self.line.lineCount - self.window.wh / self.line.lineHeight)
        case GLFW_KEY_KP_ADD =>
        case GLFW_KEY_EQUAL =>
          self.setScale(self.state.scale + 1)
        case GLFW_KEY_KP_SUBTRACT =>
        case GLFW_KEY_MINUS =>
          self.setScale(self.state.scale - 1)
        case GLFW_KEY_0 =>
        case GLFW_KEY_KP_0 =>
          if (self.state.ctrlDown)
            self.setScale(0)
        case GLFW_KEY_B =>
          self.line.lineBBEnabled = !self.line.lineBBEnabled

        case GLFW_KEY_K =>
          self.line.kerningEnabled = !self.line.kerningEnabled
    end if
  )
  glfwSetScrollCallback(self.window.window, (_: Long, _: Double, yoffset: Double) =>
    if (self.state.ctrlDown)
      self.setScale(self.state.scale + yoffset.round.toInt)
    else
      self.setLineOffset(self.state.lineOffset - yoffset.round.toInt * 3)
  )
end registerCallbacks

def framebufferSizeChanged(window: Long, width: Int, height: Int): Unit =
  glViewport(0, 0, width, height)
end framebufferSizeChanged

def windowSizeChanged2(self : FontDemo, window: Long, widthIn: Int, heightIn: Int): Unit =
  var width = widthIn
  var height = heightIn
  if Platform.get ne Platform.MACOSX then
    width = (width / self.state.contentScaleX).toInt
    height = (height / self.state.contentScaleY).toInt
  end if

  self.window.ww = width
  self.window.wh = height
  glMatrixMode(GL_PROJECTION)
  glLoadIdentity()
  glOrtho(0.0, width, height, 0.0, -1.0, 1.0)
  glMatrixMode(GL_MODELVIEW)
  self.setLineOffset(self.state.lineOffset)
end windowSizeChanged2