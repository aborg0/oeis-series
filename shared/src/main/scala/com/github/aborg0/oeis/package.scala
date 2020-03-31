package com.github.aborg0

import com.github.aborg0.oeis.eval.Evaluator.EvalContext
import com.github.aborg0.oeis.parser.ExpressionParser.ParseContext

package object oeis {
  def fromEvalContext(ctx: EvalContext): ParseContext = ParseContext(ctx.numCtx.keys.map(_.v).toSet, ctx.funcCtx.keys.map(_.name).toSet)

}
