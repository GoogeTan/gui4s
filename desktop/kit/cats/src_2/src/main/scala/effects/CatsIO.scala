package gui4s.desktop.kit.cats
package effects

import cats.effect.*
import cats.data.*
import glfw4s.core.types.*

type CatsIO[T] = EitherT[IO, GlfwError, T]


