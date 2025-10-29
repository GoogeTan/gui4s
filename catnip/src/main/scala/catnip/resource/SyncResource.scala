package catnip
package resource

trait SyncResource[Resource[_]]:
  def make[T](f : () => (T, () => Unit)) : Resource[T]
  
  def fromAutoCloseable[T <: AutoCloseable](value : () => T) : Resource[T]
end SyncResource
