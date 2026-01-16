package gui4s.android.kit

final class ScalacacheCache[IO[_], K, V](cache : scalacache.Cache[IO, K, V]) extends Cache[IO, K, V]:
  override def get(key: K): IO[Option[V]] =
    cache.get(key)
  end get

  override def getOrPut(key : K, value : IO[V]) : IO[V] =
    cache.cachingF(key)(None)(value)
  end getOrPut
end ScalacacheCache

object ScalacacheCache:
  def apply[F[_] : Sync, K <: AnyRef, V]() : Resource[F, ScalacacheCache[F, K, V]] =
    Resource.eval(scalacache.caffeine.CaffeineCache[F, K, V]).map(new ScalacacheCache(_))
  end apply
end ScalacacheCache

