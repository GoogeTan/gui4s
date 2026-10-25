package catnip
package resource

import catnip.transformer.MonadTransformer
import cats.Monad
import cats.effect.*

trait SyncResource[Resource[_]]:
  @SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
  def make[T](
    f : => (T, () => Unit),
    allocateStrategy : WrapStrategy = WrapStrategy.Delay,
    deallocateStrategy : WrapStrategy = WrapStrategy.Delay
  ) : Resource[T]
  
  def fromAutoCloseable[T <: AutoCloseable](value : => T, allocateStrategy : WrapStrategy = WrapStrategy.Delay) : Resource[T]
end SyncResource

object SyncResource:
  given[F[_] : Sync as S] : SyncResource[Resource[F, *]] with
    override def make[T](
      f: => (T, () => Unit),
      allocateStrategy: WrapStrategy,
      deallocateStrategy: WrapStrategy
    ): Resource[F, T] =
      Resource.make(
        WrapStrategy.wrapImpure(f, allocateStrategy)
      )((_, dealloc) =>
        WrapStrategy.wrapImpure(dealloc(), deallocateStrategy)
      ).map(_._1)
    end make

    override def fromAutoCloseable[T <: AutoCloseable](value: => T, allocateStrategy: WrapStrategy): Resource[F, T] =
      Resource.fromAutoCloseable(WrapStrategy.wrapImpure(value, allocateStrategy))
    end fromAutoCloseable
  end given

  given[MT[_[_], _] : MonadTransformer as MT, F[_] : {SyncResource as FSR, Monad}] : SyncResource[MT[F, *]] with
    override def fromAutoCloseable[T <: AutoCloseable](value: => T, allocateStrategy: WrapStrategy): MT[F, T] =
      MT.liftK(
        FSR.fromAutoCloseable(value, allocateStrategy)
      )
    end fromAutoCloseable

    override def make[T](f: => (T, () => Unit), allocateStrategy: WrapStrategy, deallocateStrategy: WrapStrategy): MT[F, T] =
      MT.liftK(
        FSR.make(f, allocateStrategy, deallocateStrategy)
      )
    end make
  end given

  def make[Resource[_] : SyncResource as S, T](
    f : => (T, () => Unit),
    allocateStrategy : WrapStrategy = WrapStrategy.Delay,
    deallocateStrategy : WrapStrategy = WrapStrategy.Delay
  ) : Resource[T] =
    S.make(f, allocateStrategy, deallocateStrategy)
  end make

  def fromAutoCloseable[Resource[_] : SyncResource as S, T <: AutoCloseable](
    value: => T,
    allocateStrategy: WrapStrategy = WrapStrategy.Delay
  ): Resource[T] =
    S.fromAutoCloseable(value, allocateStrategy)
  end fromAutoCloseable
end SyncResource
