package com.github.aborg0.oeis.eval

import com.github.aborg0.oeis.Expression._
import com.github.aborg0.oeis.eval.Evaluator.EvalContext
import com.github.aborg0.oeis.eval.EvaluatorMemo.MemoizedContext
import com.github.aborg0.oeis.{BoolExpression, Expression}
import monocle._
import monocle.macros._

abstract class Evaluator {
  // Possibly non-thread-safe
  final def evaluate(expressions: Seq[Expression],
               ctx: EvalContext): (Seq[Option[Expression.T]], EvalContext) =
    expressions.foldLeft((Seq.empty[Option[Expression.T]], ctx)) {
      case ((res, currCtx), expr) =>
        expr match {
          case fun @ FunDef(name, _, _) =>
            (res :+ None) -> currCtx.copy(
              funcCtx = currCtx.funcCtx.updated(name, fun))
          case _ => (res :+ Some(evaluate(expr, currCtx))) -> currCtx
        }
    }
  // Possibly non-thread-safe
  final def evaluate(expression: Expression, ctx: EvalContext): Expression.T =
    expression match {
      case Const(t) => t
      case variable @ Var(v) =>
        ctx.numCtx.getOrElse(variable,
                             throw new IllegalStateException(
                               s"Variable not found: $v, ctx: ${ctx.numCtx}"))
      case Sum(expressions @ _*) => expressions.map(evaluate(_, ctx)).sum
      case Product(expressions @ _*) =>
        expressions.map(evaluate(_, ctx)).product
      case Power(base, exponent) =>
        val b   = evaluate(base, ctx)
        val exp = evaluate(exponent, ctx)
        def safeMult(a: Int, b: Int): Int = {
          val res = a.toLong * b.toLong
          if (res <= Int.MaxValue && res >= Int.MinValue) a * b
          else
            throw new IllegalStateException(s"Overflow: $a $b, res: $res")
        }
        def fastPow(b: Int, e: Int): Int = {
          if (e < 0) throw new IllegalStateException(s"Negative exponent: $exp")
          else if (e == 0) 1
          else if (e == 1) b
          else if (e % 2 == 0) fastPow(safeMult(b, b), e / 2)
          else safeMult(b, fastPow(safeMult(b, b), e / 2))
        }
        fastPow(b, exp)
      case Minus(from, num) => evaluate(from, ctx) - evaluate(num, ctx)
      case Div(num, denom)  => evaluate(num, ctx) / evaluate(denom, ctx)
      case Mod(num, denom)  => evaluate(num, ctx) % evaluate(denom, ctx)
      case FunRef(funcName, arg) =>
        evaluateFunction(ctx, funcName, arg)
      //    case Apply(variable, value, expression) => evaluate(expression, ctx.copy(numCtx = ctx.numCtx.updated(variable, value)))
      case IfElse(pred, trueValue, falseValue) =>
        evaluate(if (evaluate(pred, ctx)) trueValue else falseValue, ctx)
      case Cases(base, cases @ _*) =>
        evaluate(
          cases
            .foldLeft(None: Option[Expression]) {
              case (res, Case(condition, expression)) =>
                if (res.isEmpty && evaluate(condition, ctx)) Some(expression)
                else res
            }
            .getOrElse(base),
          ctx
        )
    }

  protected def evaluateFunction(ctx: EvalContext, funcName: FuncName, arg: Expression): T = {
    val FunDef(_, variable, expression: Expression) = ctx.funcCtx.getOrElse(
      funcName,
      throw new IllegalStateException(
        s"Not bound variable: $funcName (in ctx: ${ctx.numCtx})"))
    evaluate(
      expression,
      ctx.copy(numCtx = ctx.numCtx.updated(variable, evaluate(arg, ctx))))
  }

  // Possibly non-thread-safe
  final def evaluate(boolExpression: BoolExpression, ctx: EvalContext): Boolean =
    boolExpression match {
      case BoolExpression.True            => true
      case BoolExpression.False           => false
      case BoolExpression.Not(expression) => !evaluate(expression, ctx)
      case BoolExpression.Or(expressions @ _*) =>
        expressions.map(evaluate(_, ctx)).reduce(_ || _)
      case BoolExpression.And(expressions @ _*) =>
        expressions.map(evaluate(_, ctx)).reduce(_ && _)
      case BoolExpression.Imply(antecedent, consequence) =>
        !evaluate(antecedent, ctx) || evaluate(consequence, ctx)
      case BoolExpression.Equal(left, right) =>
        evaluate(left, ctx) == evaluate(right, ctx)
      case BoolExpression.Less(left, right) =>
        evaluate(left, ctx) < evaluate(right, ctx)
      case BoolExpression.LessOrEqual(left, right) =>
        evaluate(left, ctx) <= evaluate(right, ctx)
      case BoolExpression.Greater(left, right) =>
        evaluate(left, ctx) > evaluate(right, ctx)
      case BoolExpression.GreaterOrEqual(left, right) =>
        evaluate(left, ctx) >= evaluate(right, ctx)
    }
}

object Evaluator extends Evaluator {
  case class EvalContext(numCtx: Map[Var, T], funcCtx: Map[FuncName, FunDef])
}