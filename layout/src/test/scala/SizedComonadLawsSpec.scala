// scala
package me.katze.gui4s.layout

import cats.kernel.Eq
import cats.laws.discipline.ComonadTests
import org.scalatest.funsuite.AnyFunSuiteLike
import org.typelevel.discipline.scalatest.FunSuiteDiscipline
import org.scalacheck.{Arbitrary, Cogen, Gen}
import me.katze.gui4s.geometry.Rect
import cats.instances.all.given
import org.scalatest.prop.Configuration.PropertyCheckConfiguration
import org.scalatestplus.scalacheck.{Checkers, ScalaCheckPropertyChecks}


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
        s <- rectArbitrary[MU].arbitrary
      } yield Sized(a, s)
    )

  given sizedCogen[MU: Cogen, A: Cogen]: Cogen[Sized[MU, A]] =
    Cogen[(A, MU, MU)].contramap(sz => (sz.value, sz.size.width, sz.size.height))

  given sizedEq[MU, A]: Eq[Sized[MU, A]] =
    Eq.fromUniversalEquals

  checkAll(
    "Sized Comonad laws (Float)",
    ComonadTests[Sized[Float, *]].comonad[Int, Long, String]
  )(using PropertyCheckConfiguration())
}
