package com.github.aborg0.oeis.eval

import com.github.aborg0.oeis.Expression
import com.github.aborg0.oeis.Expression.{FuncName, T}
import com.github.aborg0.oeis.eval.Evaluator.EvalContext
import com.github.aborg0.oeis.eval.EvaluatorMemo.MemoizedContext

class EvaluatorMemo(private var ctx: MemoizedContext) extends Evaluator {
  override protected def evaluateFunction(ctx: EvalContext, funcName: FuncName, arg: Expression): T = {
    val argRes = evaluate(arg, ctx)
    this.ctx.memo.getOrElse((ctx, funcName, argRes), {
      val res = super.evaluateFunction(ctx, funcName, arg)
      this.ctx = this.ctx.copy(memo = this.ctx.memo.updated((ctx, funcName, argRes), res))
      res
    })
  }
}

object EvaluatorMemo {
  private case class MemoizedContext(ctx: EvalContext, memo: Map[(EvalContext, FuncName, T), T])
  def apply(evalCtx: EvalContext = EvalContext(Map.empty, Map.empty)): Evaluator = {
    new EvaluatorMemo(new MemoizedContext(evalCtx, Map.empty))
  }
}