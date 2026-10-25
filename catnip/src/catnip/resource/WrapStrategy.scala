package catnip.resource

import cats.effect.Sync

/**
 * Describes how the effect library should wrapp the code
 */
enum WrapStrategy:
  case Delay
  case Blocking
  case Interruptible
  case InterruptibleMany
end WrapStrategy

object WrapStrategy:
  def wrapImpure[F[_] : Sync as S, T](value : => T, strategy: WrapStrategy) : F[T] =
    strategy match
      case WrapStrategy.Delay => S.delay(value)
      case WrapStrategy.Blocking => S.blocking(value)
      case WrapStrategy.Interruptible => S.interruptible(value)
      case WrapStrategy.InterruptibleMany => S.interruptibleMany(value)
    end match
  end wrapImpure
end WrapStrategy
