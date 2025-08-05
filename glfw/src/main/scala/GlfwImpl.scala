package me.katze.gui4s.glfw

import catnip.ForeighFunctionInterface
import cats.MonadError
import cats.effect.std.Dispatcher
import cats.effect.{Async, Resource, Sync}
import cats.syntax.all.*
import org.lwjgl.glfw.Callbacks.glfwFreeCallbacks
import org.lwjgl.glfw.GLFW.*
import org.lwjgl.glfw.GLFWErrorCallback
import org.lwjgl.system.MemoryUtil

// TODO Сделать типизированные ошибки, а не использовать MonadThrow из Sync.
final class GlfwImpl[F[_] : {ForeighFunctionInterface as impure, Sync}] extends Glfw[F, Long, OglGlfwWindow]:
  override def swapInterval(interval: Int): F[Unit] =
    impure.delay:
      glfwSwapInterval(interval)
  end swapInterval

  override def primaryMonitor : F[Long] =
    impure.delay(glfwGetPrimaryMonitor())
      .ensure(RuntimeException("Monitor is null!!"))(_ != MemoryUtil.NULL)
  end primaryMonitor

  override def createWindow(settings: WindowCreationSettings[Float]): Resource[F, OglGlfwWindow] =
    Resource.make(
      for
        _ <- windowHints(settings.visible, settings.resizeable, settings.debugContext)
        id <- createWindowId(settings.size.width.toInt, settings.size.height.toInt, settings.title, MemoryUtil.NULL /* TODO check if monitor should be passed */)
      yield OglGlfwWindow(id)
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
      glfwWindowHint(GLFW_VISIBLE, visible.toGlfwBoolean)
      glfwWindowHint(GLFW_RESIZABLE, resizable.toGlfwBoolean)
      glfwWindowHint(GLFW_OPENGL_DEBUG_CONTEXT, debugContext.toGlfwBoolean)
  end windowHints

  extension (value : Boolean)
    def toGlfwBoolean : Int =
      if value then GLFW_TRUE else GLFW_FALSE
    end toGlfwBoolean
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
        val res = new GlfwImpl[F]()
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

