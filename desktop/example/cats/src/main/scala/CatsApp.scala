package gui4s.desktop.example.cats

import catnip.resource.*
import cats.{Functor, ~>}
import cats.data.EitherT
import cats.effect.*
import glfw4s.catz.CatsPostInit
import glfw4s.core.WindowCreationSettings
import glfw4s.core.types.GlfwError
import glfw4s.core.impure.SafeImpurePostInit
import glfw4s.jvm.types.{GLFWmonitor, GLFWwindow}
import glfw4s.jvm.{JvmImpurePostInit, JvmImpurePreInit}
import gui4s.desktop.kit.common.effects.*
import gui4s.desktop.kit.common.widgets.DesktopWidget
import gui4s.desktop.kit.common.{SkijaBackend, desktopApp}
import glfw4s.catz.delayOnTheFirstThreadErrorProve

trait CatsApp extends IOApp:
  type PreInit
  
  def preInit(
               backend: SkijaBackend[CatsIO, Resource[CatsIO, *], IO, GLFWmonitor, GLFWwindow, DownEvent]
             ): Resource[CatsIO, PreInit]
  
  val settings : WindowCreationSettings[GLFWmonitor, GLFWwindow]

  final override def run(args: List[String]): IO[ExitCode] =
    CatsPostInit(
      JvmImpurePreInit,
      SafeImpurePostInit(JvmImpurePostInit),
      MainThread
    )()(using runtime).use(postInit =>

      desktopApp[
        CatsIO,
        Resource[CatsIO, *],
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
    ).foldF(
      error => IO.raiseError(new Exception("Error in glfw: " + error.description)),
      IO.pure,
    )
  end run

  given SyncResource[Resource[CatsIO, *]] with
    override def make[T](f: () => (T, () => Unit)): Resource[CatsIO, T] =
      val preInitDelay: [K] => (() => K) => IO[K] = [K] => f => IO.delay(f()).evalOn(MainThread)
      val delay = delayOnTheFirstThreadErrorProve(JvmImpurePreInit, preInitDelay)
      Resource(
        delay(
          () =>
            val (a, destr) = f()
            (a, delay(destr))
        )
      )
    end make

    override def fromAutoCloseable[T <: AutoCloseable](value: () => T): Resource[CatsIO, T] =
      Resource.fromAutoCloseable[CatsIO, T](EitherT.liftF(IO.delay(value())))
    end fromAutoCloseable
  end given

  given catzUse: Use[Resource[CatsIO, *], IO] with
    override def useResource[T, B](value: Resource[CatsIO, T])(f: T => IO[B]): IO[B] =
      value.use(f.andThen(EitherT.liftF)).foldF(
        error => IO.raiseError(new Exception("Error in glfw: " + error.description)),
        IO.pure,
      )
    end useResource
  end catzUse

  given[F[_]](using MonadCancel[F, Throwable]) : Use[Resource[F, *], F] with
    override def useResource[T, B](value: Resource[F, T])(f: T => F[B]): F[B] =
      value.use(f)
    end useResource
  end given

  given eval[F[_]] : Eval[Resource[F, *], F] with
    override def evalK: F ~> Resource[F, *] =
      Resource.liftK
    end evalK
  end eval

  given evalIO[F[_] : LiftIO as L]: Eval[Resource[F, *], IO] with
    override def evalK: IO ~> Resource[F, *] =
      new ~>[IO, Resource[F, *]]:
        override def apply[A](fa: IO[A]): Resource[F, A] =
          Resource.eval(L.liftIO(fa))
        end apply
      end new
    end evalK
  end evalIO

  // TODO REMOVE THIS WAR CRIME
  given allocateThrowing : Allocate[Resource[CatsIO, *], IO] with
    override def allocate[T](value: Resource[CatsIO, T]): IO[(T, IO[Unit])] =
      raiseError(value.allocated).map((value, destr) => (value, raiseError(destr)))
    end allocate

    def raiseError[T](value : CatsIO[T]) : IO[T] =
      value.foldF(
        error => IO.raiseError(new Exception("Error in glfw: " + error.description)),
        IO.pure,
      )
    end raiseError
  end allocateThrowing

  given allocate[F[_]](using MonadCancel[F, Throwable]) : Allocate[Resource[F, *], F] with
    override def allocate[T](value: Resource[F, T]): F[(T, F[Unit])] =
      value.allocated
    end allocate
  end allocate

  given makeF[F[_] : {LiftIO as L, Functor}] : Make[Resource[F, *], IO] with
    override def make[T](value: IO[(T, IO[Unit])]): Resource[F, T] =
      Resource(L.liftIO(value.map((value, destr) => (value, L.liftIO(destr)))))
    end make
  end makeF

  given make[F[_] : Functor] : Make[Resource[F, *], F] with
    override def make[T](value: F[(T, F[Unit])]): Resource[F, T] =
      Resource(value)
    end make
  end make

  type CatsIO[T] = EitherT[IO, GlfwError, T]

  def main(preInit: PreInit): DesktopWidget[CatsIO, ApplicationRequest]
end CatsApp