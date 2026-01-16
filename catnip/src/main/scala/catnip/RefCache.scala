package catnip

import cats.effect.{Concurrent, Deferred, Ref}
import cats.syntax.all.*

// Implementation using Ref and Deferred for atomic "getOrPut"
class RefCache[F[_], K, V](
  state: Ref[F, Map[K, Deferred[F, Either[Throwable, V]]]]
)(using F: Concurrent[F]) extends Cache[F, K, V] {

  override def get(key: K): F[Option[V]] =
    state.get.flatMap { map =>
      map.get(key) match {
        case Some(deferred) =>
          // Wait for the result and unwrap the Either
          deferred.get.flatMap(F.fromEither).map(Some(_))
        case None =>
          F.pure(None)
      }
    }

  override def getOrPut(key: K, value: F[V]): F[V] = {
    F.deferred[Either[Throwable, V]].flatMap { newDeferred =>
      // 1. Atomically check and modify the map
      state.modify { map =>
        map.get(key) match {
          case Some(existingDeferred) =>
            // Key exists (or is being computed); return map as-is and the existing wait action
            (map, existingDeferred.get.flatMap(F.fromEither))

          case None =>
            // Key missing; insert our new Deferred and return the compute action
            (map + (key -> newDeferred), computeAndComplete(newDeferred, value))
        }
      }.flatten
    }
  }

  // Helper to run the value effect and complete the deferred
  private def computeAndComplete(
    deferred: Deferred[F, Either[Throwable, V]],
    effect: F[V]
  ): F[V] = {
    // We use attempt to catch errors so we can complete the Deferred with them
    // This ensures waiting fibers don't hang if the computation fails.
    effect.attempt.flatMap { result =>
      deferred.complete(result) *> F.fromEither(result)
    }
    // Note: For a robust production cache, consider handling cancellation 
    // (uncancelable) or removing the key from the Ref on failure.
  }
}

object RefCache {
  // smart constructor
  def of[F[_]: Concurrent, K, V]: F[Cache[F, K, V]] =
    Ref.of[F, Map[K, Deferred[F, Either[Throwable, V]]]](Map.empty)
      .map(new RefCache(_))
}