package com.github.aborg0.oeis.eval

import com.github.aborg0.oeis.Expression.{Const, FunRef, FuncName}
import com.github.aborg0.oeis.eval.Evaluator.EvalContext
import org.scalatest.concurrent.{Signaler, ThreadSignaler, TimeLimitedTests}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.time.{Millis, Span}

class EvaluatorMemoTest extends AnyFunSpec with TimeLimitedTests {

  override def timeLimit: Span = Span(500, Millis)

  override val defaultTestSignaler: Signaler = ThreadSignaler

  describe("EvaluatorMemo") {
    describe("Fibonacci (A000045)") {
      it("should not take too long to compute recursive functions") {
        assert(
          EvaluatorMemo().evaluate(
            FunRef(Left(FuncName("A000045")), Const(44)),
            EvalContext.withSupportedFunctions) === 701408733)
      }
    }
  }
}
