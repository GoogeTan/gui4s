package me.katze.gui4s.skija

import cats.Monad
import cats.effect.std.Dispatcher
import cats.effect.{Async, IO, IOApp, Resource}
import cats.syntax.all.*
import io.github.humbleui.skija.*
import io.github.humbleui.skija.shaper.Shaper
import me.katze.gui4s.glfw
import me.katze.gui4s.glfw.{Glfw, GlfwImpl}
import me.katze.gui4s.impure.Impure
import me.katze.gui4s.impure.cats.effect.given
import org.lwjgl.opengl.GL.*
import org.lwjgl.opengl.{GL11, GL30}

final case class Size(width: Int, height: Int)
final case class RenderResources(
  context: DirectContext,
  surface: Surface,
  canvas: Canvas,
  font: Font,
  paint: Paint,
  shaper: Shaper
)

def makeDirectContext[F[_] : {Impure as I, Async}]: Resource[F, DirectContext] =
  Resource.fromAutoCloseable(I(DirectContext.makeGL()))

def wrapBackendRenderTarget[F[_] : {Impure as I, Async}]
    (context: DirectContext, width: Int, height: Int, frameBufferId: Int): Resource[F, Surface] =
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

def createRenderResources[F[_] : {Impure as I, Async}](size: me.katze.gui4s.glfw.Size): Resource[F, RenderResources] =
  for
    context <- makeDirectContext
    fbId <- Resource.eval(I(GL11.glGetInteger(GL30.GL_FRAMEBUFFER_BINDING)))
    surface <- wrapBackendRenderTarget(context, size.width, size.height, fbId)
    res <- Resource.eval[F, RenderResources](
      I:
        val canvas = surface.getCanvas
        val font = new Font(Typeface.makeDefault(), 32.0f)
        val paint = new Paint().setColor(0xFF000000)
        val shaper = Shaper.makeShapeDontWrapOrReorder()
        RenderResources(context, surface, canvas, font, paint, shaper)
    )
  yield res

def renderFrame[F[_] : Impure as I](resources: RenderResources): F[Unit] = I:
  val RenderResources(context, _, canvas, font, paint, shaper) = resources
  canvas.clear(0xFFFFFFFF)
  val blob = shaper.shape("Привет, Skija!", font)
  canvas.drawTextBlob(blob, 0, 0, paint)
  context.flush()

def renderLoop[F[_] : {Monad, Impure as I}, Window](window: Window, resources: RenderResources, glfw: Glfw[F, Window]): F[Unit] =
  val renderIteration = for
    _ <- renderFrame(resources)
    _ <- glfw.swapBuffers(window)
    _ <- glfw.pollEvents
  yield ()

  Monad[F].whileM_(glfw.shouldClose(window).map(!_))(renderIteration)

object SimpleSkijaExampleMonadic extends IOApp.Simple:
  def run: IO[Unit] =
    val windowSize = glfw.Size(640, 480)

    (for
      glfw <- GlfwImpl[IO]()
      _ <- Resource.eval(glfw.createPrintErrorCallback)
      window <- glfw.createWindow(
        "Skija Text Example",
        windowSize,
        visible = true,
        resizeable = false,
        debugContext = false
      )
      _ <- Resource.eval(glfw.createOGLContext(window, IO(createCapabilities())))
      resources <- createRenderResources(windowSize)
      _ <- Resource.eval(renderLoop(window, resources, glfw))
    yield ()).useForever.evalOn(MainThread)
end SimpleSkijaExampleMonadic