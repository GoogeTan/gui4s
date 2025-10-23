package gui4s.desktop.kit.zio

import gui4s.desktop.kit.common.SkijaBackend
import gui4s.desktop.kit.common.effects.DownEvent
import glfw4s.core.WindowCreationSettings
import glfw4s.core.types.GlfwError
import glfw4s.jvm.types.*
import glfw4s.zio.ZioAppWithMainThreadExecutor
import zio.*

trait Gui4sZioApp extends ZioAppWithMainThreadExecutor:
  type PreInit 
  
  val settings : WindowCreationSettings[GLFWwindow, GLFWwindow]

  def preInit(backend : SkijaBackend[IO[GlfwError, *], ZIO[Scope, GlfwError, *], UIO, GLFWmonitor, GLFWwindow, DownEvent]) : ZIO[Scope, GlfwError, PreInit]
  
  override def run(args: List[String]): ZIO[Any, Throwable, ExitCode] =
    /*
    gui4s.desktop.kit.common.desktopApp[
      IO[GlfwError, *],
      ZIO[Scope, GlfwError, *],
      UIO,
      GLFWmonitor,
      GLFWwindow,
      PreInit
    ](
      preInit = preInit,
      main = ???, //main,
      drawLoopExecutionContext = MainThreadApp.mainThread,
      updateLoopExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(10)),
      settings = settings,
      glfw = ???,
      liftIO = new ~>[UIO, IO[GlfwError, *]] {
        override def apply[A](fa: UIO[A]): IO[GlfwError, A] =
          fa
        end apply
      }
    )*/
    ZIO.succeed(ExitCode.success)
  end run

  /*
    valueToRun =>
    Unsafe.unsafe { u =>
    given u.type = u
    Runtime.default.unsafe.run[Throwable, Unit](valueToRun) match
    case Exit.Success(zioResult) =>
    ()
    case Exit.Failure(cause) =>
    cause.defects.foreach { defect =>
    defect.printStackTrace()
    }
    sys.exit(1)
    }
   */
  
  //def main(preInit : PreInit) : DesktopWidget[ApplicationRequest]
end Gui4sZioApp

