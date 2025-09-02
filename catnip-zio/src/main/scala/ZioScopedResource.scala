package catnip.zio

import cats.effect.kernel.Resource
import zio.*
import zio.interop.catz.*
import cats.effect.ExitCode as CatsExitCode
import zio.ExitCode as ZioExitCode

@SuppressWarnings(Array("org.wartremover.warts.Any"))
def zioScopedToResource[A](zioScoped: ZIO[Scope, Throwable, A]): Resource[Task, A] =
  Resource.makeCase {
    for {
      scope <- Scope.make
      a     <- zioScoped.provideEnvironment(ZEnvironment(scope))
    } yield (a, scope)
  } { case ((_, scope), exitCase) =>
    scope.close(exitCaseToZioExit(exitCase)).unit
  }.map(_._1)
end zioScopedToResource

def exitCaseToZioExit(exitCase : Resource.ExitCase) : Exit[Throwable, Any] =
  exitCase match
    case Resource.ExitCase.Succeeded => Exit.unit
    case Resource.ExitCase.Errored(e) => Exit.fail(e)
    case Resource.ExitCase.Canceled => Exit.interrupt(FiberId.None)
  end match
end exitCaseToZioExit

@SuppressWarnings(Array("org.wartremover.warts.Any"))
def resourceToScoped[A](res: Resource[Task, A]): ZIO[Scope, Throwable, A] =
  for
    (a, release) <- res.allocated
    _ <- ZIO.addFinalizer(release.orDie)
  yield a
end resourceToScoped

extension (ec: CatsExitCode)
  def toZIO: ZioExitCode = ZioExitCode(ec.code)
end extension

extension (ec: ZioExitCode)
  def toCats: CatsExitCode = CatsExitCode(ec.code)
end extension
  