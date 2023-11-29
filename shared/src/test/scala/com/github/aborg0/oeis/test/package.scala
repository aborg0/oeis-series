package com.github.aborg0.oeis

import org.scalacheck.{Arbitrary, Shrink}
import org.scalactic.Prettifier
import org.scalactic.source.Position
import org.scalatest.enablers.WheneverAsserting
import org.scalatest.prop.Configuration
import org.scalatestplus.scalacheck.{CheckerAsserting, ScalaCheckDrivenPropertyChecks}

package object test {
  implicit class ScalaCheckDrivenPropertyChecksOps(
      val self: ScalaCheckDrivenPropertyChecks)
      extends AnyVal {
    def forAllDistinct[A, ASSERTION](fun: A => ASSERTION)(
        implicit config: Configuration#PropertyCheckConfiguration,
        arbA: Arbitrary[A],
        shrA: Shrink[A],
        asserting: CheckerAsserting[ASSERTION],
        prettifier: Prettifier,
        pos: Position): Unit /*asserting.Result*/ = {
      val seen = scala.collection.mutable.Set.empty[A]
      import WheneverAsserting._
      implicit val configX: self.PropertyCheckConfiguration = config.asInstanceOf[self.PropertyCheckConfiguration]
      self.forAll { (n: A) =>
        self.whenever(!seen.contains(n))({
          seen += n
          fun
        })
      } //(config, arbA, shrA, asserting, prettifier, pos)
    }
    def forAllDistinct[A, B, ASSERTION](fun: (A, B) => ASSERTION)(
        implicit config: Configuration#PropertyCheckConfiguration,
        arbA: Arbitrary[A],
        arbB: Arbitrary[B],
        shrA: Shrink[A],
        shrB: Shrink[B],
        asserting: CheckerAsserting[ASSERTION],
        prettifier: Prettifier,
        pos: Position): Unit /*asserting.Result*/ = {
      val seen = scala.collection.mutable.Set.empty[(A, B)]
      import WheneverAsserting._
      implicit val configX: self.PropertyCheckConfiguration = config.asInstanceOf[self.PropertyCheckConfiguration]
      self.forAll { (a: A, b: B) =>
        self.whenever(!seen.contains(a -> b))({
          seen += ((a, b))
          fun
        })
      } //(config, arbA, shrA, asserting, prettifier, pos)
    }
  }
}
