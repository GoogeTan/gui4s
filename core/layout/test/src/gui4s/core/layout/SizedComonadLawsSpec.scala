package gui4s.core.layout

import cats.instances.all.given
import cats.kernel.Eq
import cats.laws.discipline.ComonadTests
import gui4s.core.geometry.Rect
import org.scalacheck.{Arbitrary, Cogen}
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatestplus.scalacheck.Checkers
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

final class SizedComonadLawsSpec extends AnyFunSuiteLike with FunSuiteDiscipline with Checkers {
  // Arbitrary, Cogen, Eq для Rect
  given rectArbitrary[MU: Arbitrary]: Arbitrary[Rect[MU]] =
    Arbitrary(
      for {
        w <- Arbitrary.arbitrary[MU]
        h <- Arbitrary.arbitrary[MU]
      } yield Rect(w, h)
    )

  given rectCogen[MU: Cogen]: Cogen[Rect[MU]] =
    Cogen[(MU, MU)].contramap(r => (r.width, r.height))

  given rectEq[MU]: Eq[Rect[MU]] =
    Eq.fromUniversalEquals

  // Arbitrary, Cogen, Eq для Sized
  given sizedArbitrary[MU: Arbitrary, A: Arbitrary]: Arbitrary[Sized[MU, A]] =
    Arbitrary(
      for {
        a <- Arbitrary.arbitrary[A]
        s <- Arbitrary.arbitrary[MU]
      } yield Sized(a, s)
    )

  given sizedCogen[MU: Cogen, A: Cogen]: Cogen[Sized[MU, A]] =
    Cogen[(A, MU)].contramap(sz => (sz.value, sz.size))

  given sizedEq[MU, A]: Eq[Sized[MU, A]] =
    Eq.fromUniversalEquals

  checkAll(
    "Sized Comonad laws (Float)",
    ComonadTests[Sized[Float, *]].comonad[Int, Long, String]
  )(using PropertyCheckConfiguration())
}
