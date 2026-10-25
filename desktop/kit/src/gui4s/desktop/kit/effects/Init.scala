package gui4s.desktop.kit.effects

import cats.*
import cats.arrow.FunctionK
import cats.effect.*

import gui4s.core.widget.library.decorator.Decorator

import gui4s.desktop.kit.widgets.DesktopWidget

opaque type Init[T] = Resource[IO, (Decorator[DesktopWidget[Nothing]], T)]

object Init:
  def evalResource[T](init: Resource[IO, T]): Init[T] =
    init.map(t => (identity, t))

  def liftResource: Resource[IO, *] ~> Init =
    new (Resource[IO, *] ~> Init):
      def apply[A](fa: Resource[IO, A]): Init[A] =
        evalResource(fa)
      end apply
    end new
  end liftResource

  def liftIO: IO ~> Init =
    new FunctionK[IO, Init]:
      def apply[A](fa: IO[A]): Init[A] = eval(fa)
    end new
  end liftIO

  def eval[T](init: IO[T]): Init[T] =
    evalResource(Resource.eval(init))
    
  def emitDecorator[T](dec: Decorator[DesktopWidget[Nothing]]): Init[Unit] =
    Resource.pure((dec, ()))
  end emitDecorator

  def pure[T](init: T): Init[T] =
    Resource.pure((identity, init))

  def run[T](init: Init[T]): Resource[IO, (Decorator[DesktopWidget[Nothing]], T)] =
    init
  end run

  def runWidget(init: Init[DesktopWidget[Nothing]]): Resource[IO, DesktopWidget[Nothing]] =
    init.map(_(_))

  given initIsAMonad : MonadThrow[Init] with
    override def pure[A](x: A): Init[A] = Init.pure(x)

    override def flatMap[A, B](fa: Init[A])(f: A => Init[B]): Init[B] =
      fa.flatMap((dec, a) => f(a).map((dec2, b) => (dec compose dec2, b)))

    override def tailRecM[A, B](a: A)(f: A => Init[Either[A, B]]): Init[B] =
      Monad[Resource[IO, *]].tailRecM[
        (Decorator[DesktopWidget[Nothing]], A),
        (Decorator[DesktopWidget[Nothing]], B)
      ]((identity, a))((dec, a) => f(a).map:
        case (dec2, Left(a)) => Left((dec compose dec2, a))
        case (dec2, Right(b)) => Right((dec compose dec2, b))
      )
    end tailRecM

    override def raiseError[A](e: Throwable): Init[A] =
      Resource.raiseError(e)
    end raiseError

    override def handleErrorWith[A](fa: Init[A])(f: Throwable => Init[A]): Init[A] =
      fa.handleErrorWith(f)
    end handleErrorWith
  end initIsAMonad
end Init
