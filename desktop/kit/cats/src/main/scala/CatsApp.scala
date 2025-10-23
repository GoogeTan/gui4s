package gui4s.desktop.kit.cats

import effects.{ApplicationRequest, DownEvent}
import widgets.DesktopWidget

import cats.data.EitherT
import cats.effect.{ExitCode, IO, IOApp, Resource}
import glfw4s.catz.CatsPostInit
import glfw4s.core.WindowCreationSettings
import glfw4s.core.types.GlfwError
import glfw4s.jvm.types.{GLFWmonitor, GLFWwindow}
import glfw4s.jvm.{JvmImpurePostInit, JvmImpurePreInit}
import gui4s.desktop.kit.common.{SkijaBackend, desktopApp}

trait CatsApp extends IOApp:
  type PreInit
  
  def preInit(
               backend: SkijaBackend[EitherT[IO, GlfwError, *], Resource[EitherT[IO, GlfwError, *], *], IO, GLFWmonitor, GLFWwindow, DownEvent]
             ): Resource[EitherT[IO, GlfwError, *], PreInit]
  
  val settings : WindowCreationSettings[GLFWmonitor, GLFWwindow]

  override def run(args: List[String]): IO[ExitCode] =
    CatsPostInit(
      JvmImpurePreInit,
      JvmImpurePostInit,
      MainThread
    )()(using runtime).use(postInit =>
      desktopApp[
        EitherT[IO, GlfwError, *],
        Resource[EitherT[IO, GlfwError, *], *],
        IO,
        GLFWmonitor,
        GLFWwindow,
        PreInit
      ](
        preInit = preInit,
        main = main,
        updateLoopExecutionContext = this.runtime.compute,
        drawLoopExecutionContext = MainThread,
        settings = settings,
        glfw = postInit,
        liftIO = EitherT.liftK
      )
    )
  end run

  def main(preInit: PreInit): DesktopWidget[ApplicationRequest]
end CatsApp