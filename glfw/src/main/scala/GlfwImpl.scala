package me.katze.gui4s.glfw

import catnip.ForeighFunctionInterface
import cats.MonadError
import cats.effect.std.Dispatcher
import cats.effect.{Async, Resource, Sync}
import cats.syntax.all.*
import org.lwjgl.glfw.Callbacks.glfwFreeCallbacks
import org.lwjgl.glfw.GLFW.*
import org.lwjgl.glfw.GLFWErrorCallback
import org.lwjgl.system.{MemoryStack, MemoryUtil}

import java.util.Objects

def stackPush[F[_] : {Sync}](ffi : ForeighFunctionInterface[F]): Resource[F, MemoryStack] =
  Resource.fromAutoCloseable(ffi.delay(MemoryStack.stackPush()))
end stackPush

final case class OglGlfwWindow[F[_] : Sync](id : Long, impure : ForeighFunctionInterface[F], unsafeRunF: [A] => F[A] => A) extends GlfwWindow[F, Long]:
  override def center: F[Unit] =
    size.flatMap:
      case Size(width, height) =>
        impure.delay:
          val monitor = glfwGetPrimaryMonitor()
          val vidmode = Objects.requireNonNull(glfwGetVideoMode(monitor))
          glfwSetWindowPos(
            id,
            (vidmode.width - width) / 2,
            (vidmode.height - height) / 2
          )
  end center


  override def windowResizeCallback(callback: Size => F[Unit]): F[Unit] =
    impure.delay:
      glfwSetWindowSizeCallback(
        id,
        (_, width, height) =>
          unsafeRunF(callback(Size(width, height)))
      )
  end windowResizeCallback

  override def makeVisible: F[Unit] =
    impure.delay:
      glfwShowWindow(id)
  end makeVisible

  override def shouldClose: F[Boolean] =
    impure.delay:
      glfwWindowShouldClose(id)
  end shouldClose

  override def markForBeingClosed: F[Unit] =
    impure.delay:
      glfwSetWindowShouldClose(id, true)
  end markForBeingClosed

  override def frameBufferResizeCallback(callback: Size => F[Unit]): F[Unit] =
    impure.delay:
      val old = glfwSetFramebufferSizeCallback(id, (_, width, height) =>
        unsafeRunF(callback(Size(width, height)))
      )
      if old != null then
        old.free()
      end if
  end frameBufferResizeCallback

  override def keyCallback(callback: (key : Int, scanCode : Int, keyAction : KeyAction, keyModes : KeyModes) => F[Unit]): F[Unit] =
    impure.delay:
      val old = glfwSetKeyCallback(id, (_, key, scancode, action, modes) =>
        unsafeRunF(callback(key, scancode, KeyAction.fromCode(action), KeyModes.fromMask(modes)))
      )
      if old != null then
        old.free()
      end if
  end keyCallback

  override def monitor : F[Long] =
    impure.delay(glfwGetWindowMonitor(id)).ensure(RuntimeException("Monitor is null!!"))(_ != MemoryUtil.NULL)
  end monitor


  override def size: F[Size] =
    stackPush(impure).use:
      stack =>
        impure.delay({
          val width = stack.mallocInt(1)
          val height = stack.mallocInt(1)
          glfwGetWindowSize(id, width, height)
          Size(width.get(0), height.get(0))
        })
  end size

  override def frameBufferSize: F[Size] =
    stackPush(impure).use:
      stack =>
        impure.delay({
          val width = stack.mallocInt(1)
          val height = stack.mallocInt(1)
          glfwGetFramebufferSize(id, width, height)
          Size(width.get(0), height.get(0))
        })
  end frameBufferSize

  override def scrollCallback(callback: (xoffset : Double, yoffset : Double) => F[Unit]): F[Unit] =
    impure.delay:
      val old = glfwSetScrollCallback(id, (_, xoffset, yoffset) => unsafeRunF[Unit](callback(xoffset, yoffset)))
      if old != null then
        old.free()
      end if
  end scrollCallback

  override def cursorPosCallback(callback: (newXPos : Double, newYPos : Double) => F[Unit]): F[Unit] =
    impure.delay:
      val old = glfwSetCursorPosCallback(id, (_, xpos, ypos) => unsafeRunF[Unit](callback(xpos, ypos)))
      if old != null then
        old.free()
      end if
  end cursorPosCallback

  override def mouseButtonCallback(callback: (key : Int, action : KeyAction, mode : KeyModes) => F[Unit]): F[Unit] =
    impure.delay:
      val old = glfwSetMouseButtonCallback(id, (_, key, action, modes) =>
        unsafeRunF(callback(key, KeyAction.fromCode(action), KeyModes.fromMask(modes)))
      )
      if old != null then
        old.free()
      end if
  end mouseButtonCallback

  override def currentMousePosition: F[(Double, Double)] =
    stackPush(impure).use:
      stack =>
        impure.delay:
          val x = stack.mallocDouble(1)
          val y = stack.mallocDouble(1)
          glfwGetCursorPos(id, x, y)
          (x.get(0) * 2.0, y.get(0) * 2.0) // TODO Это странная дичь, тут надо умножать на скейл монитора. Пока что так.
  end currentMousePosition

  override def swapBuffers: F[Unit] =
    impure.delay:
      glfwSwapBuffers(id)
  end swapBuffers
end OglGlfwWindow

final class GlfwImpl[F[_] : {ForeighFunctionInterface as impure, Sync}](unsafeRunF : [A] => F[A] => A) extends Glfw[F, OglGlfwWindow[F]]:

  override def swapInterval(interval: Int): F[Unit] =
    impure.delay:
      glfwSwapInterval(interval)
  end swapInterval

  override type Monitor = Long

  override def createOGLContext(window : OglGlfwWindow[F], createCapabilities: F[Unit]): F[Unit] =
    impure.delay(
      glfwMakeContextCurrent(window.id)
    ) *> createCapabilities
  end createOGLContext

  override def primaryMonitor : F[Monitor] =
    impure.delay(glfwGetPrimaryMonitor())
      .ensure(RuntimeException("Monitor is null!!"))(_ != MemoryUtil.NULL)
  end primaryMonitor

  override def createWindow(settings: WindowCreationSettings): Resource[F, OglGlfwWindow[F]] =
    Resource.make(
      for
        _ <- windowHints(settings.visible, settings.resizeable, settings.debugContext)
        id <- createWindowId(settings.size.width, settings.size.height, settings.title, MemoryUtil.NULL /* TODO check if monitor should be passed */)
      yield OglGlfwWindow(id, impure, unsafeRunF)
    )(a => 
        impure.delay:
          glfwFreeCallbacks(a.id)
          glfwDestroyWindow(a.id)
    )
  end createWindow

  def createWindowId(width : Int, height : Int, title : CharSequence, monitor : Long) : F[Long] =
    impure.delay(
      glfwCreateWindow(width, height, title, monitor, MemoryUtil.NULL)
    ).ensure(
      RuntimeException("Failed to create GLFW window")
    )(_ != MemoryUtil.NULL)
  end createWindowId

  def monitorScale(monitor : Long): F[Float] =
    stackPush(impure).use:
      s =>
        impure.delay:
          val px = s.mallocFloat(1)
          val py = s.mallocFloat(1)
          glfwGetMonitorContentScale(monitor, px, py)
          val (scaleX, scaleY) = (px.get(0), py.get(0))
          assert(scaleX == scaleY)
          scaleX
  end monitorScale

  override def primaryMonitorScale: F[Float] =
    primaryMonitor.flatMap(monitorScale)
  end primaryMonitorScale

  def windowHints(visible : Boolean, resizable : Boolean, debugContext : Boolean) : F[Unit] =
    impure.delay:
      glfwDefaultWindowHints()
      glfwWindowHint(GLFW_VISIBLE, visible.toGlfw)
      glfwWindowHint(GLFW_RESIZABLE, resizable.toGlfw)
      glfwWindowHint(GLFW_OPENGL_DEBUG_CONTEXT, debugContext.toGlfw)
  end windowHints

  extension (value : Boolean)
    def toGlfw : Int =
      if value then GLFW_TRUE else GLFW_FALSE
    end toGlfw
  end extension

  override def createPrintErrorCallback: Resource[F, GLFWErrorCallback] =
    Resource.make(
      impure.delay(GLFWErrorCallback.createPrint.set())
    )(
      errorCallback =>
        impure:
          glfwSetErrorCallback(null)
          errorCallback.free()
    )
  end createPrintErrorCallback

  override def pollEvents: F[Unit] =
    impure.delay(glfwPollEvents())

  def initGlfw: F[Unit] =
    impure.delay(glfwInit()).ifM(
      ().pure[F],
      MonadError[F, Throwable].raiseError(RuntimeException("Failed to create the GLFW"))
    )
  end initGlfw
  
  def terminate : F[Unit] =
    impure.delay(glfwTerminate())
  end terminate
  
end GlfwImpl

object GlfwImpl:
  def apply[F[_] : {ForeighFunctionInterface, Sync}](run : [A] => F[A] => A) : Resource[F, GlfwImpl[F]] =
    Resource.make(
      {
        val res = new GlfwImpl[F](run)
        res.initGlfw.as(res)
      }
    )(_.terminate).flatMap(impl =>
      impl.createPrintErrorCallback.as(impl)
    )
  end apply
  
  def apply[F[_] : {ForeighFunctionInterface, Sync}](dispatcher : Dispatcher[F]) : Resource[F, GlfwImpl[F]] =
    GlfwImpl[F]([A] => (effect : F[A]) => dispatcher.unsafeRunSync(effect))
  end apply
  
  def apply[F[_] : {ForeighFunctionInterface, Async}]() : Resource[F, GlfwImpl[F]] =
    Dispatcher.sequential[F].flatMap(GlfwImpl[F](_))
  end apply
end GlfwImpl

