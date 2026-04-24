package gui4s.android.kit

import cats.effect.IO
import cats.effect.kernel.Resource

final class ScalacacheCache[K, V](cache : scalacache.Cache[IO, K, V]) extends Cache[IO, K, V]:
  override def get(key: K): IO[Option[V]] =
    cache.get(key)
  end get

  override def getOrPut(key : K, value : IO[V]) : IO[V] =
    cache.cachingF(key)(None)(value)
  end getOrPut
end ScalacacheCache

object ScalacacheCache:
  def apply[K <: AnyRef, V]() : Resource[IO, ScalacacheCache[K, V]] =
    Resource.eval(scalacache.caffeine.CaffeineCache[IO, K, V]).map(new ScalacacheCache(_))
  end apply
end ScalacacheCache

