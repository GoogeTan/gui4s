package catnip

trait Zip[Collection[_]]:
  def zip[A, B](a : Collection[A], b : Collection[B]) : Collection[(A, B)]
end Zip

object Zip:
  extension[Collection[_], A](a : Collection[A])
    def zip[B](b : Collection[B])(using z : Zip[Collection]) : Collection[(A, B)] =
      z.zip(a, b)
    end zip
  end extension
end Zip
