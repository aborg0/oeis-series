package com.github.aborg0.oeis.eval

import com.github.aborg0.oeis.BoolExpression.{Equal, Greater, Less, Or}
import com.github.aborg0.oeis.Expression._
import com.github.aborg0.oeis.Expression

abstract class Evaluator {
  import Evaluator._
  // Possibly non-thread-safe
  final def evaluate(
                      expressions: Seq[Expression],
                      ctx: EvalContext): (Seq[Option[Expression.T]], EvalContext) =
    expressions.foldLeft((Seq.empty[Option[Expression.T]], ctx)) {
      case ((res, currCtx), expr) =>
        expr match {
          case fun@FunDef(name, _, _) =>
            (res :+ None) -> currCtx.copy(
              funcCtx = currCtx.funcCtx.updated(name, fun))
          case _ => (res :+ Some(evaluate(expr, currCtx))) -> currCtx
        }
    }

  // Possibly non-thread-safe
  final def evaluate(expression: Expression, ctx: EvalContext): Expression.T =
    expression match {
      case Const(t) => t
      case variable@Var(v) =>
        ctx.numCtx.getOrElse(variable,
          throw new IllegalStateException(
            s"Variable not found: $v, ctx: ${ctx.numCtx}"))
      case Sum(expressions@_*) => expressions.map(evaluate(_, ctx)).sum
      case Product(expressions@_*) =>
        expressions.map(evaluate(_, ctx)).product
      case SafeProduct(expressions@_*) =>
        expressions.map(evaluate(_, ctx)).foldLeft(1)((acc, v) => safeMult(acc, v))
      case Power(base, exponent) =>
        val b = evaluate(base, ctx)
        val exp = evaluate(exponent, ctx)

        def fastPow(b: Int, e: Int): Int = {
          if (e < 0) throw new IllegalStateException(s"Negative exponent: $exp")
          else if (e == 0) 1
          else if (e == 1) b
          else if (e % 2 == 0) fastPow(safeMult(b, b), e / 2)
          else safeMult(b, fastPow(safeMult(b, b), e / 2))
        }

        fastPow(b, exp)
      case Minus(from, num) => evaluate(from, ctx) - evaluate(num, ctx)
      case Div(num, denom) => evaluate(num, ctx) / evaluate(denom, ctx)
      case Mod(num, denom) => evaluate(num, ctx) % evaluate(denom, ctx)
      case FunRef(func, args@_*) =>
        evaluateFunction(ctx, func, args: _*)
      //    case Apply(variable, value, expression) => evaluate(expression, ctx.copy(numCtx = ctx.numCtx.updated(variable, value)))
      case IfElse(pred, trueValue, falseValue) =>
        evaluate(if (evaluate(pred, ctx)) trueValue else falseValue, ctx)
      case Cases(base, cases@_*) =>
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
      case LargerOrEqualValueInAscending(v, reference) =>
        val idx = findIndex(v, reference, ctx, _ <= _)
        evaluateFunction(ctx, reference, Const(idx))
      case LargerOrEqualIndex1InAscending(v, reference) =>
        findIndex(v, reference, ctx, _ <= _)
      case SmallerValueInAscending(v, reference) =>
        val idx = findIndex(v, reference, ctx, _ <= _) - 1
        evaluateFunction(ctx, reference, Const(idx))
      case SmallerIndex1InAscending(v, reference) =>
        findIndex(v, reference, ctx, _ <= _) - 1
    }

  import com.github.aborg0.oeis._
  protected def findIndex(compareTo: Expression, reference: Either[FuncName, FunDef], ctx: EvalContext,
                          compareToLeft: (T, T) => Boolean): Int ={
    val ref = evaluate(compareTo, ctx)
    val FunDef(_, Seq(variable), expression) = extractFunc(reference, ctx)

    val powerIdx = Iterator.iterate(1)(safeMult(_, 2))
      .find(idx => compareToLeft(ref, evaluate(expression, ctx.copy(numCtx = ctx.numCtx.updated(variable, idx)))))
      .getOrElse(throw new IllegalStateException(s"Could not find larger value"))
    powerIdx match {
      case idx@(1 | 2) => idx
      case _ => (powerIdx / 2 to powerIdx).binarySearchFirst(idx =>
        evaluate(expression, ctx.copy(numCtx = ctx.numCtx.updated(variable, idx))), ref, compareToLeft)
    }

  }

  protected def evaluateFunction(ctx: EvalContext,
                                 func: Either[FuncName, FunDef],
                                 args: Expression*): T = {
    val FunDef(_, variables, expression: Expression) = extractFunc(func, ctx)

    evaluate(
      expression, variables.zip(args).foldRight(ctx) { case ((variable, arg), ctx) =>
        ctx.copy(numCtx = ctx.numCtx.updated(variable, evaluate(arg, ctx)))
      })
  }

  protected def extractFunc(func: Either[FuncName, FunDef], ctx: EvalContext): FunDef = {
    func match {
      case Left(funcName) =>
        ctx.funcCtx.getOrElse(
          funcName,
          throw new IllegalStateException(
            s"Not bound variable: $funcName (in ctx: ${ctx.numCtx})"))
      case Right(funDef) => funDef
    }
  }

  // Possibly non-thread-safe
  final def evaluate(boolExpression: BoolExpression,
                     ctx: EvalContext): Boolean =
    boolExpression match {
      case BoolExpression.True => true
      case BoolExpression.False => false
      case BoolExpression.Not(expression) => !evaluate(expression, ctx)
      case BoolExpression.Or(expressions@_*) =>
        expressions.map(evaluate(_, ctx)).reduce(_ || _)
      case BoolExpression.And(expressions@_*) =>
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
  private def safeMult(a: Int, b: Int): Int = {
    val res = a.toLong * b.toLong
    if (res <= Int.MaxValue && res >= Int.MinValue) a * b
    else
      throw new IllegalStateException(s"Overflow: $a $b, res: $res")
  }

  case class EvalContext(numCtx: Map[Var, T], funcCtx: Map[FuncName, FunDef])

  object EvalContext {
    def withSupportedFunctions: EvalContext =
      EvalContext(
        Map.empty,
        Set(
          //http://oeis.org/A000142 factorial
          FunDef(FuncName("!"),
            Var("n") :: Nil,
            IfElse(Equal(Var("n"), Const(0)),
              Const(1),
              SafeProduct(Var("n"),
                FunRef(Left(FuncName("!")),
                  Minus(Var("n"), Const(1)))))),
          FunDef(FuncName("A000142"),
            Var("n") :: Nil,
            FunRef(Left(FuncName("!")), Var("n"))),
          //http://oeis.org/A006882 double factorial
          FunDef(FuncName("!!"),
            Var("n") :: Nil,
            IfElse(Less(Var("n"), Const(1)),
              Const(1),
              SafeProduct(Var("n"),
                FunRef(Left(FuncName("!!")),
                  Minus(Var("n"), Const(2)))))),
          FunDef(FuncName("A006882"),
            Var("n") :: Nil,
            FunRef(Left(FuncName("!!")), Var("n"))),
          FunDef(FuncName("choose"),
            Var("n") :: Var("k") :: Nil,
            IfElse(
              Or(Less(Var("k"), Const(0)), Greater(Var("k"), Var("n"))),
              Const(0),
              IfElse(Less(Var("k"), Minus(Var("n"),Var("k"))),
                Div(FunRef(Left(FuncName("choose_k_n-k_product")), Sum(Minus(Var("n"), Var("k")), Const(1))),
                FunRef(Left(FuncName("!")), Minus(Var("k"), Const(0)))),
                Div(FunRef(Left(FuncName("choose_k_n-k_product")), Sum(Var("k"), Const(1))),
                FunRef(Left(FuncName("!")), Minus(Minus(Var("n"), Var("k")), Const(0))))
              )
            )),
          //          FunDef(
          //            FuncName("choose_k"),
          //            Var("k") :: Nil,
          //
          //          ),
          FunDef(
            FuncName("choose_k_n-k_product"),
            Var("n-k") :: Nil,
            IfElse(Greater(Var("n-k"), Var("n")),
              Const(1),
              Product(Var("n-k"),
                FunRef(Left(FuncName("choose_k_n-k_product")),
                  Sum(Var("n-k"), Const(1)))))
          ),
          // http://oeis.org/A000217 triangular numbers (n*(n+1)/2)
          FunDef(FuncName("A000217"), Var("n") :: Nil, Div(Product(Var("n"), Sum(Var("n"), Const(1))), Const(2))),
          // http://oeis.org/A007318 Pascal triangle by rows
          FunDef(FuncName("A007318"), Var("n") :: Nil, FunRef(Left(FuncName("choose")),
            SmallerIndex1InAscending(Var("n"), Left(FuncName("A000217"))),
            Minus(Minus(Var("n"), Const(1)), SmallerValueInAscending(Var("n"), Left(FuncName("A000217"))))
          )),
          // http://oeis.org/A000079 powers of 2
          FunDef(FuncName("A000079"), Var("n") :: Nil, Power(Const(2), Minus(Var("n"), Const(0)))),
          // http://oeis.org/A000045 Fibonacci
          FunDef(FuncName("A000045"), List(Var("n")),IfElse(Equal(Var("n"),Const(0)),Const(0),
            IfElse(Equal(Var("n"),Const(1)),Const(1),
              Sum(
                FunRef(Left(FuncName("A000045")), Minus(Var("n"),Const(1))),
                FunRef(Left(FuncName("A000045")), Minus(Var("n"),Const(2))))))),
        ).view.map(funDef => funDef.name -> funDef).toMap
      )
  }

}
