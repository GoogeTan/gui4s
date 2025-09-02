package gui4s.desktop.example.zio

import catnip.Cache
import catnip.zio.resourceToScoped
import scalacache.caffeine.CaffeineCache
import zio.{RIO, Scope, Task, ZIO}
import zio.interop.catz.*

final class ScalacacheCache[K, V](cache: scalacache.Cache[Task, K, V]) extends Cache[Task, K, V] :
  override def get(key: K): Task[Option[V]] =
    cache.get(key)
  end get

  override def getOrPut(key: K, value: Task[V]): Task[V] =
    cache.cachingF(key)(None)(value)
  end getOrPut
end ScalacacheCache


object ScalacacheCache:
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def apply[K <: AnyRef, V]() : Task[ScalacacheCache[K, V]] =
    CaffeineCache[Task, K, V].map(new ScalacacheCache(_))
  end apply
end ScalacacheCache
