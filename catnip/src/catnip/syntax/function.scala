package catnip
package syntax

object function:
  extension [T, A, B, C](value: (A, B, C) => T)
    // TODO Я не знаю, почему встроенный метод не работает, так что так
    infix def andThen[NewT](f: T => NewT): (A, B, C) => NewT =
      (self, path, states) => f(value(self, path, states))
    end andThen
  end extension

  extension [T, A, B](value: (A, B) => T)
    // TODO Я не знаю, почему встроенный метод не работает, так что так
    infix def andThen[NewT](f: T => NewT): (A, B) => NewT =
      (self, path) => f(value(self, path))
    end andThen
  end extension
end function
