package me.katze.gui4s.draw
package test.test2

import test.GLFWUtil.glfwInvoke

import cats.effect.kernel.Sync
import cats.effect.{IO, Ref, Resource}
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

final case class OGLWindow(
                            window: Long,
                            var ww: Int,
                            var wh: Int,
                            debugProc: Callback | Null,
                          ):
  def destroy(): IO[Unit] =
    IO:
      GL.setCapabilities(null)
      if (debugProc != null)
        debugProc.free()
      glfwFreeCallbacks(window)
      glfwDestroyWindow(window)
      glfwTerminate()
      Objects.requireNonNull(glfwSetErrorCallback(null)).free()
  end destroy
end OGLWindow

final case class StyledText(
                            text : String,
                            lineCount: Int,
                            fontHeight: Int,
                          )

final case class State(
                        var scale: Int,
                        var contentScaleX: Float,
                        var contentScaleY: Float,
                        var ctrlDown: Boolean,
                        var lineOffset: Int,
                        var kerningEnabled: Boolean,
                        var lineBBEnabled: Boolean,
                      )


def lineHeight(styledText: StyledText, state: State): Float =
  styledText.fontHeight * (1.0f + state.scale * 0.25f)
end lineHeight

def lineHeight(state : FontDemo): Float =
  state.line.fontHeight * (1.0f + state.state.scale * 0.25f)
end lineHeight

def setLineOffset[F[_]](state : Ref[F, FontDemo], offset : Int) : F[Unit] =
  state.update(
    demo =>
      demo.copy(
        state = demo.state.copy(
          lineOffset = max(0, min(offset, demo.line.lineCount - (demo.window.wh / lineHeight(demo)).toInt))
        )
      )
  )
end setLineOffset

def setScale[F[_]](state : Ref[F, FontDemo], scale : Int) : F[Unit] =
  state.update(
    a =>
      a.copy(
        state = a.state.copy(
          scale = scale
        )
      )
  )
end setScale

def setLineOffset(offset: Float, state: State, line: StyledText, window: OGLWindow): Unit =
  setLineOffset(offset.round, state, line, window)
end setLineOffset

def setLineOffset(offset: Int, state : State, line : StyledText, window : OGLWindow): Unit =
  state.lineOffset = max(0, min(offset, line.lineCount - (window.wh / lineHeight(line, state)).toInt))
end setLineOffset

def setScale(scale: Int, state : State, line : StyledText, window : OGLWindow): Unit =
  state.scale = max(-3, scale)
  //line.lineHeight = line.fontHeight * (1.0f + state.scale * 0.25f)
  setLineOffset(state.lineOffset, state, line, window)

final case class FontDemo(
                            window : OGLWindow,
                            line : StyledText,
                            state : State
                          )

def stackPushResource[F[_] : Sync](using impure : Impure[F]) : Resource[F, MemoryStack] =
  Resource.fromAutoCloseable(
    impure.impure:
      stackPush
  )
end stackPushResource

def initFrameBuffer(monitor: Long, self: State, windowWidth: Int, windowHeight: Int)(using Impure[IO]) : IO[(Int, Int)] =
  stackPushResource[IO].use:
    s =>
      IO:
        val px = s.mallocFloat(1)
        val py = s.mallocFloat(1)
        glfwGetMonitorContentScale(monitor, px, py)
        self.contentScaleX = px.get(0)
        self.contentScaleY = py.get(0)
        if Platform.get eq Platform.MACOSX then
          (windowWidth, windowHeight)
        else
          (windowWidth * self.contentScaleX.round, windowHeight * self.contentScaleY.round)
end initFrameBuffer

def errorCallbackToPrint : IO[Unit] =
  IO:
    GLFWErrorCallback.createPrint.set

def ensureGlfwInit : IO[Unit] =
  (
    IO:
      glfwInit()
  ).ifM(
    IO.unit,
    IO.raiseError(new IllegalStateException("Unable to initialize GLFW"))
  )

def defaultWindowHints : IO[Unit] =
  IO:
    glfwDefaultWindowHints()
    glfwWindowHint(GLFW_VISIBLE, GLFW_FALSE)
    glfwWindowHint(GLFW_RESIZABLE, GLFW_TRUE)
    glfwWindowHint(GLFW_OPENGL_DEBUG_CONTEXT, GLFW_TRUE)

def initMonitor : IO[Long] =
  IO:
    glfwGetPrimaryMonitor

def initWindow(state : State, title : String)(using Impure[IO]) : IO[Long] =
  for
    monitor <- initMonitor
    (framebufferW, framebufferH) <- initFrameBuffer(monitor, state, 800, 600)
    res <- IO:
      val window = glfwCreateWindow(framebufferW, framebufferH, title, NULL, NULL)
      if window == NULL then
        throw new RuntimeException("Failed to create the GLFW window")
      // Center window
      val vidmode = Objects.requireNonNull(glfwGetVideoMode(monitor))
      glfwSetWindowPos(window, (vidmode.width - framebufferW) / 2, (vidmode.height - framebufferH) / 2)
      window
  yield res
end initWindow

def initWindow(state: State, line: StyledText, title: String)(using Impure[IO]): Resource[IO, OGLWindow] =
  Resource.make(
    for
      _ <- errorCallbackToPrint *> ensureGlfwInit *> defaultWindowHints
      window <- initWindow(state, title)
      res <-
        IO:
          // Create context
          glfwMakeContextCurrent(window)
          GL.createCapabilities
          val debugProc = GLUtil.setupDebugMessageCallback
          glfwSwapInterval(1)
          glfwShowWindow(window)
          val res = OGLWindow(window, 800, 600, debugProc)
          glfwInvoke(window, windowSizeChanged2(state, res, line, _, _, _), framebufferSizeChanged)
          res
      _ <- registerCallbacks(state, res, line)
    yield res
  )(
    _.destroy()
  )
end initWindow

def registerCallbacks(state: State, oglWindow: OGLWindow, line: StyledText) : IO[Unit] =
  IO:
    glfwSetWindowSizeCallback(oglWindow.window, windowSizeChanged2(state, oglWindow, line, _, _, _))
    glfwSetFramebufferSizeCallback(oglWindow.window, framebufferSizeChanged)
    glfwSetKeyCallback(oglWindow.window, (window: Long, key: Int, _: Int, action: Int, _: Int) =>
      key match
        case GLFW_KEY_LEFT_CONTROL =>
        case GLFW_KEY_RIGHT_CONTROL =>
          state.ctrlDown = action != GLFW_RELEASE
        case _ => ()
      end match

      if action != GLFW_RELEASE then
        key match
          case GLFW_KEY_ESCAPE =>
            glfwSetWindowShouldClose(window, true)
          case GLFW_KEY_PAGE_UP =>
            setLineOffset(state.lineOffset - oglWindow.wh / lineHeight(line, state), state, line, oglWindow)
          case GLFW_KEY_PAGE_DOWN =>
            setLineOffset(state.lineOffset + oglWindow.wh / lineHeight(line, state), state, line, oglWindow)
          case GLFW_KEY_HOME =>
            setLineOffset(0, state, line, oglWindow)
          case GLFW_KEY_END =>
            setLineOffset(line.lineCount - oglWindow.wh / lineHeight(line, state), state, line, oglWindow)
          case GLFW_KEY_KP_ADD =>
          case GLFW_KEY_EQUAL =>
            setScale(state.scale + 1, state, line, oglWindow)
          case GLFW_KEY_KP_SUBTRACT =>
          case GLFW_KEY_MINUS =>
            setScale(state.scale - 1, state, line, oglWindow)
          case GLFW_KEY_0 =>
          case GLFW_KEY_KP_0 =>
            if (state.ctrlDown)
              setScale(0, state, line, oglWindow)
          case GLFW_KEY_B =>
            state.lineBBEnabled = !state.lineBBEnabled

          case GLFW_KEY_K =>
            state.kerningEnabled = !state.kerningEnabled
          case _ => ()
      end if
    )
    glfwSetScrollCallback(oglWindow.window, (_: Long, _: Double, yoffset: Double) =>
      if (state.ctrlDown)
        setScale(state.scale + yoffset.round.toInt, state, line, oglWindow)
      else
        setLineOffset(state.lineOffset - yoffset.round.toInt * 3, state, line, oglWindow)
    )
end registerCallbacks

def framebufferSizeChanged(window: Long, width: Int, height: Int): Unit =
  glViewport(0, 0, width, height)
end framebufferSizeChanged

def platformIsNotMac: IO[Boolean] =
  IO:
    Platform.get ne Platform.MACOSX


def windowSizeChanged2(state: State, oglWindow: OGLWindow, line: StyledText, window: Long, widthIn: Int, heightIn: Int): Unit =
  var width = widthIn
  var height = heightIn
  if Platform.get ne Platform.MACOSX then
    width = (width / state.contentScaleX).toInt
    height = (height / state.contentScaleY).toInt
  end if

  oglWindow.ww = width
  oglWindow.wh = height
  glMatrixMode(GL_PROJECTION)
  glLoadIdentity()
  glOrtho(0.0, width, height, 0.0, -1.0, 1.0)
  glMatrixMode(GL_MODELVIEW)
  setLineOffset(state.lineOffset, state, line, oglWindow)
end windowSizeChanged2
