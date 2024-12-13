package me.katze.gui4s.draw
package lwjgl.test

import lwjgl.GLFWUtil.glfwInvoke

import cats.effect.std.Dispatcher
import cats.effect.{IO, Ref, Resource}
import org.lwjgl.glfw.Callbacks.glfwFreeCallbacks
import org.lwjgl.glfw.GLFW.*
import org.lwjgl.glfw.GLFWErrorCallback
import org.lwjgl.opengl.GL11.*
import org.lwjgl.opengl.{GL, GLUtil}
import org.lwjgl.system.MemoryStack.stackPush
import org.lwjgl.system.MemoryUtil.NULL
import org.lwjgl.system.{Callback, MemoryStack, Platform}

import java.util.Objects

final case class GlfwWindow(
                              openGlWindow : Long,
                              windowWidth  : Int,
                              windowHeight : Int,
                              contentScaleX: Float,
                              contentScaleY: Float,
                              debugProc    : Callback,
                          ):
  @SuppressWarnings(Array("org.wartremover.warts.Null"))
  def deallock : IO[Unit] =
    IO:
      GL.setCapabilities(null)
      if debugProc != null then
        debugProc.free()
      end if
      glfwFreeCallbacks(openGlWindow)
      glfwDestroyWindow(openGlWindow)
      glfwTerminate()
      Objects.requireNonNull(glfwSetErrorCallback(null)).free()
  end deallock
end GlfwWindow


@SuppressWarnings(Array("org.wartremover.warts.Null"))
def windowSizeChanged(font: Ref[IO, GlfwWindow], width: Int, height: Int): IO[Unit] =
  font.update(f =>
    if Platform.get != Platform.MACOSX then
      f.copy(
        windowWidth = (width / f.contentScaleX).toInt, 
        windowHeight = (height / f.contentScaleY).toInt
      )
    else 
      f.copy(
        windowWidth = width,
        windowHeight = height
      )
  ) *> 
    IO:
      glMatrixMode(GL_PROJECTION)
      glLoadIdentity()
      glOrtho(0.0, width, height, 0.0, -1.0, 1.0)
      glMatrixMode(GL_MODELVIEW)
end windowSizeChanged

@SuppressWarnings(Array("org.wartremover.warts.Throw"))
def initGlfw() : IO[Unit] =
  IO:
    GLFWErrorCallback.createPrint.set
    if !glfwInit then
      throw new IllegalStateException("Unable to initialize GLFW")
    end if  
    glfwDefaultWindowHints()
    glfwWindowHint(GLFW_VISIBLE, GLFW_FALSE)
    glfwWindowHint(GLFW_RESIZABLE, GLFW_TRUE)
    glfwWindowHint(GLFW_OPENGL_DEBUG_CONTEXT, GLFW_TRUE)
end initGlfw

final case class FrameBufferSize(contentScaleX : Float, contentScaleY : Float, framebufferW : Int, framebufferH : Int)

def createFrameBuffer(monitor : Long, windowWidth : Int, windowHeight : Int, s: MemoryStack) : IO[FrameBufferSize] =
  IO:
    val px = s.mallocFloat(1)
    val py = s.mallocFloat(1)
    glfwGetMonitorContentScale(monitor, px, py)
    val contentScaleX = px.get(0)
    val contentScaleY = py.get(0)
    if Platform.get == Platform.MACOSX then
      FrameBufferSize(
        contentScaleX = contentScaleX,
        contentScaleY = contentScaleY,
        framebufferW = windowWidth,
        framebufferH = windowHeight
      )
    else
      FrameBufferSize(
        contentScaleX = contentScaleX,
        contentScaleY = contentScaleY,
        framebufferW = windowWidth * contentScaleX.round,
        framebufferH = windowHeight * contentScaleY.round
      )
    end if
end createFrameBuffer

def stackPushResource : Resource[IO, MemoryStack] =
  Resource.fromAutoCloseable(IO.apply(stackPush()))
end stackPushResource

def initFrameBuffer(monitor : Long, windowWidth : Int, windowHeight : Int) : IO[FrameBufferSize] =
  stackPushResource.use(createFrameBuffer(monitor, windowWidth, windowHeight, _))
end initFrameBuffer

def glfwPrimaryMonitor : IO[Long] =
  IO:
    glfwGetPrimaryMonitor
end glfwPrimaryMonitor

@SuppressWarnings(Array("org.wartremover.warts.All"))
def createWindow(title: String, framebufferW: Int, framebufferH: Int) : IO[Long] =
  IO:
    val window = glfwCreateWindow(framebufferW, framebufferH, title, NULL, NULL)
    if (window == NULL)
      throw new RuntimeException("Failed to create the GLFW window")
    end if
    window
end createWindow

def centerWindow(window : Long, monitor : Long, framebufferW : Int, framebufferH: Int) : IO[Unit] =
  IO:
    val vidmode = Objects.requireNonNull(glfwGetVideoMode(monitor))
    glfwSetWindowPos(window, (vidmode.width - framebufferW) / 2, (vidmode.height - framebufferH) / 2)
end centerWindow

def createContext(window: Long) : IO[Callback] =
  IO:
    glfwMakeContextCurrent(window)
    GL.createCapabilities
    val debugProc = GLUtil.setupDebugMessageCallback
    glfwSwapInterval(1)
    glfwShowWindow(window)
    debugProc
end createContext

def init(title: String, windowWidth: Int, windowHeight: Int) : IO[GlfwWindow] =
  for
    _ <- initGlfw()
    monitor <- glfwPrimaryMonitor
    tmp <- initFrameBuffer(monitor, windowWidth, windowHeight)
    FrameBufferSize(contentScaleX, contentScaleY, framebufferW, framebufferH) = tmp
    window <- createWindow(title, framebufferW, framebufferH)
    _ <- centerWindow(window, monitor, framebufferW, framebufferH)
    debugProc <- createContext(window)
  yield GlfwWindow(window, windowWidth, windowHeight, contentScaleX, contentScaleY, debugProc)
end init

type KeyCallback = (Ref[IO, GlfwWindow], Int, Int, Int, Int) => IO[Unit]
type ScrollCallback = (Ref[IO, GlfwWindow], Double, Double) => IO[Unit]

def registerCallBacks(ref: Ref[IO, GlfwWindow], dispatcher: Dispatcher[IO], keyCallback: KeyCallback, scrollCallback: ScrollCallback) : IO[Unit] =
  ref.get.flatMap:
    window  =>
      IO:
        glfwSetWindowSizeCallback(
          window.openGlWindow,
          (_, width, height) => dispatcher.unsafeRunSync(windowSizeChanged(ref, width, height)),
        )
        glfwSetFramebufferSizeCallback(window.openGlWindow, (_, width, height) => glViewport(0, 0, width, height))
      
        glfwInvoke(
          window.openGlWindow,
          (_, width, height) => dispatcher.unsafeRunSync(windowSizeChanged(ref, width, height)),
          (_, width, height) => glViewport(0, 0, width, height)
        )
        glfwSetKeyCallback(window.openGlWindow, 
          (_: Long, key: Int, scancode: Int, action: Int, mods: Int) =>
            dispatcher.unsafeRunSync(keyCallback(ref, key, scancode, action, mods))
        )
        glfwSetScrollCallback(window.openGlWindow,
          (_: Long, xoffset: Double, yoffset: Double) =>
            dispatcher.unsafeRunSync(scrollCallback(ref, xoffset, yoffset))
        )
end registerCallBacks

def initWindowResource(dispatcher: Dispatcher[IO], title: String, windowWidth: Int, windowHeight: Int, keyCallback: KeyCallback, scrollCallback: ScrollCallback) : Resource[IO, Ref[IO, GlfwWindow]] =
  Resource.make(
    for
      realWindow <- init(title, windowWidth, windowHeight)
      ref <- Ref.of[IO, GlfwWindow](realWindow)
      _ <- registerCallBacks(ref, dispatcher, keyCallback, scrollCallback)
    yield ref  
  )(
    ref => ref.get flatMap (_.deallock)
  )
end initWindowResource  