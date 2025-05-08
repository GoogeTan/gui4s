package me.katze.gui4s.skija

import cats.effect.{Async, IO, IOApp, Resource}
import cats.syntax.all.*
import cats.{Functor, Monad}
import io.github.humbleui.skija.*
import io.github.humbleui.skija.shaper.Shaper
import me.katze.gui4s.impure.Impure
import me.katze.gui4s.impure.cats.effect.given
import org.lwjgl.glfw.GLFW.*
import org.lwjgl.glfw.GLFWErrorCallback
import org.lwjgl.opengl.GL.*
import org.lwjgl.opengl.{GL, GL11, GL30}
import org.lwjgl.system.MemoryUtil.NULL

final case class WindowConfig(width: Int, height: Int, title: String)
final case class RenderResources(
  context: DirectContext,
  surface: Surface,
  canvas: Canvas,
  font: Font,
  paint: Paint,
  shaper: Shaper
)

final case class Window(
                          descriptor : Long,
                          width: Int,
                          height: Int,
                          errorCallback: GLFWErrorCallback
                        )


def initGLFW[F[_] : {Impure as I, Functor}]: Resource[F, Unit] =
  Resource.eval(I(require(glfwInit())))
end initGLFW

def createErrorCallback[F[_] : {Impure as I, Async}] : Resource[F, GLFWErrorCallback] =
  Resource.fromAutoCloseable(
    I:
      val errorCallback = GLFWErrorCallback.createPrint(System.err)
      glfwSetErrorCallback(errorCallback)
      errorCallback
  )
end createErrorCallback

def createWindow[F[_] : {Impure as I, Async}](config: WindowConfig): Resource[F, Window] =
  createErrorCallback.flatMap((errorCallback) =>
    Resource.eval(
      I:
        glfwWindowHint(GLFW_VISIBLE, GLFW_TRUE)
        glfwWindowHint(GLFW_RESIZABLE, GLFW_FALSE)
        glfwWindowHint(GLFW_STENCIL_BITS, 8)

        val errorCallback = GLFWErrorCallback.createPrint(System.err)
        glfwSetErrorCallback(errorCallback)

        val window = glfwCreateWindow(config.width, config.height, config.title, NULL, NULL)
        if window == NULL then
          val error = glfwGetError(null)
          throw IllegalStateException(s"Failed to create GLFW window. Error code: $error")
        Window(window, config.width, config.height, errorCallback)
    )
  )
end createWindow

def setupOpenGL[F[_] : Impure as I](window: Window): F[Unit] = I:
  glfwMakeContextCurrent(window.descriptor)
  createCapabilities()
end setupOpenGL

def makeDirectContext[F[_] : {Impure as I, Async}] : Resource[F, DirectContext] =
  Resource.fromAutoCloseable(
    I:
      DirectContext.makeGL()
  )
end makeDirectContext

def wrapBackendRenderTarget[F[_] : {Impure as I, Async}](context : DirectContext, width : Int, height : Int, frameBufferId : Int) : Resource[F, Surface] =
  Resource.fromAutoCloseable(
    I:
      Surface.wrapBackendRenderTarget(
        context,
        BackendRenderTarget.makeGL(
          width, height, 0, 8, frameBufferId,
          FramebufferFormat.GR_GL_RGBA8
        ),
        SurfaceOrigin.BOTTOM_LEFT,
        SurfaceColorFormat.RGBA_8888,
        ColorSpace.getSRGB
      )
  )
end wrapBackendRenderTarget

def createRenderResources[F[_] : {Impure as I, Async}](width: Int, height: Int): Resource[F, RenderResources] =
  for
    context <- makeDirectContext
    fbId <- Resource.eval(I(GL11.glGetInteger(GL30.GL_FRAMEBUFFER_BINDING)))
    surface <- wrapBackendRenderTarget(context, width, height, fbId)
    res <- Resource.eval[F, RenderResources](
      I:
        val canvas = surface.getCanvas
        val font = new Font(Typeface.makeDefault(), 32.0f)
        val paint = new Paint().setColor(0xFF000000)
        val shaper = Shaper.makeShapeDontWrapOrReorder()

        RenderResources(context, surface, canvas, font, paint, shaper)
    )
  yield res
end createRenderResources

def renderFrame[F[_] : Impure as I](resources: RenderResources): F[Unit] = I:
  val RenderResources(context, _, canvas, font, paint, shaper) = resources

  canvas.clear(0xFFFFFFFF)
  val blob = shaper.shape("Привет, Skija!", font)
  canvas.drawTextBlob(blob, 0, 0, paint)
  context.flush()
end renderFrame


def swapBuffers[F[_] : Impure as I](window: Long): F[Unit] = I:
  glfwSwapBuffers(window)
end swapBuffers

def pollEvents[F[_] : Impure as I]: F[Unit] = I:
  glfwPollEvents()
end pollEvents

def shouldWindowClose[F[_] : Impure as I](window: Long): F[Boolean] = I:
  glfwWindowShouldClose(window)
end shouldWindowClose

def renderLoop[F[_] : {Monad, Impure as I}](window: Window, resources: RenderResources): F[Unit] =
  val renderIteration = for
    _ <- renderFrame(resources)
    _ <- swapBuffers(window.descriptor)
    _ <- pollEvents
  yield ()

  Monad[F].whileM_(shouldWindowClose(window.descriptor).map(a => !a))(renderIteration)
end renderLoop

object SimpleSkijaExampleMonadic extends IOApp.Simple:
  def run: IO[Unit] =
    val config = WindowConfig(640, 480, "Skija Text Example")

    (for
      _ <- initGLFW
      window <- createWindow(config)
      _ <- Resource.eval(setupOpenGL(window))
      resources <- createRenderResources(config.width, config.height)
      _ <- Resource.eval(renderLoop[IO](window, resources))
    yield ()).useForever.evalOn(MainThread)
  end run
end SimpleSkijaExampleMonadic
