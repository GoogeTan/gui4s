package me.katze.gui4s.draw
package lwjgl.test2

import lwjgl.FontDemo
import lwjgl.GLFWUtil.glfwInvoke
import org.lwjgl.glfw.Callbacks.glfwFreeCallbacks
import org.lwjgl.glfw.GLFW.*
import org.lwjgl.glfw.{GLFWErrorCallback, GLFWVidMode}
import org.lwjgl.opengl.GL11.*
import org.lwjgl.opengl.{GL, GLUtil}
import org.lwjgl.system.MemoryStack.stackPush
import org.lwjgl.system.MemoryUtil.NULL
import org.lwjgl.system.{Callback, MemoryStack, Platform}

import java.lang.Math.{max, min}
import java.nio.FloatBuffer
import java.util.Objects
import java.util.regex.{Matcher, Pattern}
import scala.util.Using

final case class FontDemo(
                            val text: String,
                            var lineCount: Int,
                            var window: Long,
                            var ww: Int,
                            var wh: Int,
                            var contentScaleX: Float,
                            var contentScaleY: Float,
                            var ctrlDown: Boolean,
                            val fontHeight: Int,
                            var scale: Int,
                            var lineOffset: Int,
                            var lineHeight: Float,
                            var kerningEnabled: Boolean,
                            var lineBBEnabled: Boolean,
                            var debugProc: Callback | Null,
                          ):
  def setLineOffset(offset: Float): Unit =
    setLineOffset(offset.round)

  def setLineOffset(offset: Int): Unit =
    lineOffset = max(0, min(offset, lineCount - (wh / lineHeight).toInt))
  
  def setScale(scale: Int): Unit =
    this.scale = max(-3, scale)
    this.lineHeight = fontHeight * (1.0f + this.scale * 0.25f)
    setLineOffset(lineOffset)

  def destroy(): Unit =
    GL.setCapabilities(null)
    if (debugProc != null)
      debugProc.free()
    glfwFreeCallbacks(window)
    glfwDestroyWindow(window)
    glfwTerminate()
    Objects.requireNonNull(glfwSetErrorCallback(null)).free()
  end destroy
end FontDemo

object FontDemo:
  @SuppressWarnings(Array("org.wartremover.warts.All"))
  def apply(text : String, fontHeight : Int) : FontDemo =
    var lc = 0
    val m = Pattern.compile("^.*$", Pattern.MULTILINE).matcher(text)
    while (m.find)
      lc += 1
    new FontDemo(
      text,
      lc,
      0,
      800,
      600,
      .0,
      .0,
      false,
      fontHeight,
      0,
      0,
      0f,
      true,
      false,
      null
    )
  end apply
end FontDemo

def initFD(self: FontDemo, title: String): Unit =
  GLFWErrorCallback.createPrint.set
  if (!glfwInit)
    throw new IllegalStateException("Unable to initialize GLFW")
  glfwDefaultWindowHints()
  glfwWindowHint(GLFW_VISIBLE, GLFW_FALSE)
  glfwWindowHint(GLFW_RESIZABLE, GLFW_TRUE)
  glfwWindowHint(GLFW_OPENGL_DEBUG_CONTEXT, GLFW_TRUE)
  val monitor = glfwGetPrimaryMonitor
  var framebufferW = 0
  var framebufferH = 0
  Using(stackPush):
    s =>
      val px = s.mallocFloat(1)
      val py = s.mallocFloat(1)
      glfwGetMonitorContentScale(monitor, px, py)
      self.contentScaleX = px.get(0)
      self.contentScaleY = py.get(0)
      if (Platform.get eq Platform.MACOSX)
      {
        framebufferW = self.ww
        framebufferH = self.wh
      }
      else
      {
        framebufferW = self.ww * self.contentScaleX.round
        framebufferH = self.wh * self.contentScaleY.round
      }
  self.window = glfwCreateWindow(framebufferW, framebufferH, title, NULL, NULL)
  if (self.window == NULL)
    throw new RuntimeException("Failed to create the GLFW window")
  glfwSetWindowSizeCallback(self.window, windowSizeChanged2(self, _, _, _))
  glfwSetFramebufferSizeCallback(self.window, framebufferSizeChanged)
  glfwSetKeyCallback(self.window, (window: Long, key: Int, scancode: Int, action: Int, mods: Int) =>
      key match
        case GLFW_KEY_LEFT_CONTROL =>
        case GLFW_KEY_RIGHT_CONTROL =>
          self.ctrlDown = action != GLFW_RELEASE
      end match

      if action != GLFW_RELEASE then
        key match
          case GLFW_KEY_ESCAPE =>
            glfwSetWindowShouldClose(window, true)
          case GLFW_KEY_PAGE_UP =>
            self.setLineOffset(self.lineOffset - self.wh / self.lineHeight)
          case GLFW_KEY_PAGE_DOWN =>
            self.setLineOffset(self.lineOffset + self.wh / self.lineHeight)
          case GLFW_KEY_HOME =>
            self.setLineOffset(0)
          case GLFW_KEY_END =>
            self.setLineOffset(self.lineCount - self.wh / self.lineHeight)
          case GLFW_KEY_KP_ADD =>
          case GLFW_KEY_EQUAL =>
            self.setScale(self.scale + 1)
          case GLFW_KEY_KP_SUBTRACT =>
          case GLFW_KEY_MINUS =>
            self.setScale(self.scale - 1)
          case GLFW_KEY_0 =>
          case GLFW_KEY_KP_0 =>
            if (self.ctrlDown)
              self.setScale(0)
          case GLFW_KEY_B =>
            self.lineBBEnabled = !self.lineBBEnabled

          case GLFW_KEY_K =>
            self.kerningEnabled = !self.kerningEnabled
      end if
  )
  glfwSetScrollCallback(self.window, (window: Long, xoffset: Double, yoffset: Double) =>
  {
    if (self.ctrlDown)
      self.setScale(self.scale + yoffset.round.toInt)
    else
      self.setLineOffset(self.lineOffset - yoffset.round.toInt * 3)
  })
  // Center window
  val vidmode = Objects.requireNonNull(glfwGetVideoMode(monitor))
  glfwSetWindowPos(self.window, (vidmode.width - framebufferW) / 2, (vidmode.height - framebufferH) / 2)
  // Create context
  glfwMakeContextCurrent(self.window)
  GL.createCapabilities
  self.debugProc = GLUtil.setupDebugMessageCallback
  glfwSwapInterval(1)
  glfwShowWindow(self.window)
  glfwInvoke(self.window, windowSizeChanged2(self, _, _, _), framebufferSizeChanged)
end initFD

def framebufferSizeChanged(window: Long, width: Int, height: Int): Unit =
  glViewport(0, 0, width, height)
end framebufferSizeChanged

def windowSizeChanged2(self : FontDemo, window: Long, widthIn: Int, heightIn: Int): Unit =
  var width = widthIn
  var height = heightIn
  if Platform.get ne Platform.MACOSX then
    width = (width / self.contentScaleX).toInt
    height = (height / self.contentScaleY).toInt
  end if

  self.ww = width
  self.wh = height
  glMatrixMode(GL_PROJECTION)
  glLoadIdentity()
  glOrtho(0.0, width, height, 0.0, -1.0, 1.0)
  glMatrixMode(GL_MODELVIEW)
  self.setLineOffset(self.lineOffset)
end windowSizeChanged2