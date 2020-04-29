package com.github.aborg0.oeis.eval

import com.github.aborg0.oeis.Expression
import com.github.aborg0.oeis.Expression.{FunDef, FuncName, T}
import com.github.aborg0.oeis.eval.Evaluator.EvalContext
import com.github.aborg0.oeis.eval.EvaluatorMemo.MemoizedContext
import monocle._
import monocle.macros._

class EvaluatorMemo(private var ctx: MemoizedContext) extends Evaluator {
  import EvaluatorMemo._
  override protected def evaluateFunction(ctx: EvalContext, func: Either[FuncName, FunDef], args: Expression*): T = {
    func match {
      case Left(funcName) =>
        val (argsRes, _) = args.foldRight((Seq.empty[T], ctx)) { case (arg, (prevVars, ctx)) =>
          val value = evaluate(arg, ctx)
          (value +: prevVars) -> ctx//.copy(numCtx = ctx.numCtx.updated(variable, value))
        }
        this.ctx.memo.getOrElse((ctx, funcName, argsRes), {
          val res = super.evaluateFunction(ctx, func, args: _*)
          this.ctx = memoLens.modify(_.updated((ctx, funcName, argsRes), res))(this.ctx)
          res
        })
      case _ => super.evaluateFunction(ctx, func, args: _*)
    }
  }
}

object EvaluatorMemo {
  private val memoLens = GenLens[MemoizedContext](_.memo)

  // TODO memoize set enumerations too
  private case class MemoizedContext(ctx: EvalContext, memo: Map[(EvalContext, FuncName, Seq[T]), T])
  def apply(evalCtx: EvalContext = EvalContext.empty): Evaluator = {
    new EvaluatorMemo(MemoizedContext(evalCtx, Map.empty))
  }
}