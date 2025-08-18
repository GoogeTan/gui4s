package catnip

trait Zip[Container[_]]:
  def zip[A, B](a : Container[A], b : Container[B]) : Container[(A, B)]
end Zip

object Zip:
  extension[Container[_], A](a : Container[A])
    def zip[B](b : Container[B])(using z : Zip[Container]) : Container[(A, B)] =
      z.zip(a, b)
    end zip
  end extension
end Zip
