package gui4s.desktop.kit

import scala.reflect.Typeable
import catnip.syntax.all.{*, given}
import cats.*
import cats.arrow.FunctionK
import cats.data.EitherT
import cats.data.ReaderT
import cats.effect.*
import cats.effect.std.*
import cats.effect.unsafe.IORuntime
import cats.syntax.all.*
import glfw4s.core.GlfwConstants.*
import glfw4s.core.WindowCreationSettings
import glfw4s.core.pure.*
import glfw4s.core.types.GlfwError
import glfw4s.jna.bindings.structs.GLFWcursor
import glfw4s.jna.bindings.types.GLFWmonitor
import glfw4s.jna.bindings.types.GLFWwindow
import glfw4s.jvm.CatsJvmPostInit
import io.github.humbleui.skija.Canvas
import gui4s.core.geometry.*
import gui4s.core.widget.*
import gui4s.desktop.kit.*
import gui4s.desktop.kit.effects.Draw.given
import gui4s.desktop.kit.effects.*
import gui4s.desktop.kit.widgets.*
import gui4s.desktop.skija.DirectContext.*
import gui4s.desktop.skija.canvas.clear
import gui4s.desktop.widget.library.*

enum UIAppError:
  case InitError(glfwError : GlfwError)
  case PlaceError(exception : Throwable)

  override def toString: String =
    this match
      case InitError(GlfwError(code, description, trace)) => "InitError(" + code + ", " + description + ", " + trace + ")"
      case PlaceError(exception) => "PlaceError(" + exception + ")"
  end toString
end UIAppError

trait UIApp extends IOApp:
  final given Typeable[IO[Unit]] = (a : Any) =>
    a match
      case _ : IO[c] =>
        Some(a.asInstanceOf[IO[Unit] & a.type])
      case _ => None
    end match
  end given

  val settings : WindowCreationSettings[GLFWmonitor, GLFWwindow]

  final override def run(args: List[String]): IO[ExitCode] =
    runResourced.use_.as(ExitCode.Success)
  end run

  final def runResourced : Resource[IO, ExitCode] =
    given IORuntime = runtime
    for
      glfw <- CatsJvmPostInit(
        MainThread,
        error => IO.raiseError(new Exception("Error in GLFW: " + error.toString))
      )()
      //Skija требует новый OGL, без этого будут иногда падать шейдеры
      _ <- glfw.windowHint(GLFW_CONTEXT_VERSION_MAJOR, 3).eval
      _ <- glfw.windowHint(GLFW_CONTEXT_VERSION_MINOR, 3).eval
      _ <- glfw.windowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE).eval
      _ <- glfw.windowHint(GLFW_OPENGL_FORWARD_COMPAT, 1).eval
      window <- glfw.createWindow(settings)
      _ <- glfw.makeContextCurrent(Some(window)).eval

      eventBus <- Queue.unbounded[IO, DownEvent].to[IO].eval
      _ <- glfw.makeContextCurrent(Some(window)).eval

      eventBus <- Queue.unbounded[IO, DownEvent].to[IO].eval
      _ <- glfw.makeContextCurrent(Some(window)).eval
      surface <- SkijaSurface.create(window, glfw, FunctionK.id).evalOn(MainThread)
      _ <- glfw.addFramebufferSizeCallback(
        window,
        (_ : GLFWwindow, w : Int, h : Int) =>
          surface.recreateRenderTarget(Rect(w, h))
            *> eventBus.offer(DownEvent.WindowShouldBeRedrawn)
      ).eval

      runPlaceK : (PlaceC[IO] ~> IO) = Place.run[IO](Path(Nil), windowBounds(window, glfw))

      runDrawK : (Draw[IO] => IO[Boolean]) =
        draw =>
          surface.drawFrame(sur =>
            for
              _ <- clear[ReaderT[IO, Canvas, *]](0xFFFFFFFF).run(sur.canvas)
              _ <- draw.run(sur.canvas)
              _ <- flush[IO](sur.directContext)
              _ <- glfw.swapBuffers(window)
              _ <- glfw.pollEvents
            yield (),
            FunctionK.id
          ) *> glfw.windowShouldClose(window).map(!_)

      widgetCell <- main(glfw, window, eventBus).evalMap(freeMainWidget =>
        Ref.ofEffect(runWidgetForTheFirstTime(freeMainWidget, runPlaceK, RecompositionReaction.run))
      )
      exitCode <- desktopWidgetLoops[IO, IO, Nothing](
        runDraw =  runDrawK,
        runPlace = runPlaceK,
        waitForTheNextEvent = eventBus.take,
        updateLoopExecutionContext = this.runtime.compute,
        drawLoopExecutionContext = MainThread,
        widget = widgetCell,
      ).eval
    yield exitCode
  end runResourced

  def windowBounds(window: GLFWwindow, glfw : PureWindow[IO, IO[Unit], GLFWmonitor, GLFWwindow]): IO[Bounds] =
    glfw.getFramebufferSize(window).map((w, h) => Rect(new InfinityOr(w.toFloat), new InfinityOr(h.toFloat)))
  end windowBounds

  def main(
            glfw: PurePostInit[IO, IO[Unit], GLFWmonitor, GLFWwindow, GLFWcursor, Int],
            window: GLFWwindow,
            eventBus: Queue[IO, DownEvent],
  ): Resource[IO, DesktopWidget[IO, Nothing]]
end UIApp