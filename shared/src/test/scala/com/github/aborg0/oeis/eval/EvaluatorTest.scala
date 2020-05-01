package com.github.aborg0.oeis.eval

import com.github.aborg0.oeis.Expression.{Const, FunRef, FuncName, Mod}
import com.github.aborg0.oeis.eval.Evaluator.EvalContext
import com.github.aborg0.oeis.test.Tags
import org.scalatest.funspec.AnyFunSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class EvaluatorTest extends AnyFunSpec with ScalaCheckDrivenPropertyChecks {
  describe("Evaluator") {
    describe("built-in AST operations") {
      describe("mod") {
        forAll { (i: Int, j: Int) =>
          whenever(j != 0) {
            it(s"should compute correct values for $i, $j",
               Tags.Stable,
               Tags.Generated) {
              assert(Evaluator.evaluate(Mod(Const(i), Const(j)),
                                        EvalContext.empty) === i % j)
            }
          }
        }
      }
    }
    describe("default functions") {
      val ctx = Evaluator.EvalContext.withSupportedFunctions
      describe("gcd") {
        val gcdName = Left(FuncName("gcd"))
        def gcdEval(n: Int, m: Int): Int =
          Evaluator.evaluate(FunRef(gcdName, Const(n), Const(m)), ctx)

        it("should handle the n<m case") {
          assert(gcdEval(6, 9) === 3)
        }

        it(s"should compute n for any positive n, when m=0", Tags.Generated) {
          forAll { n: Int =>
            whenever(n >= 0) {
              assert(gcdEval(n, 0) === n)
            }
          }
        }

        it(s"should compute 6 for 48 and 18") {
          assert(gcdEval(48, 18) === 6)
        }

        forAll { (n: Int, m: Int) =>
          whenever(n > 0 && m > 0) {
            it(s"should agree with gcd of BigInts for $n $m", Tags.Generated) {
              assert(gcdEval(n, m) === BigInt(n).gcd(BigInt(m)).intValue)
            }
          }
        }
      }
//      describe("choose") {
//        def chooseEval(n: Int, k: Int): Int =
//          Evaluator.evaluate(FunRef(Left(FuncName("choose")),
//                                    Const(n),
//                                    Const(k)),
//                             ctx)
//
//        (0 to 44).foreach { n: Int =>
//          it(s"should be 1 for k=0 n=$n", Tags.Generated) {
//            assert(chooseEval(n, 0) === 1)
//          }
//          it(s"should be 1 for k=n, n=$n", Tags.Generated) {
//            assert(chooseEval(n, n) === 1)
//          }
//          it(s"should be n for k=1 n=$n", Tags.Generated) {
//            assert(chooseEval(n, 1) === n)
//          }
//          it(s"should be n for k=n-1 n=$n", Tags.Generated) {
//            assert(chooseEval(n, n - 1) === n)
//          }
//        }
//      }
    }
  }
}
