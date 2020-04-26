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
          //http://oeis.org/A000142
          // TODO maybe implement with safeMult if it gets supported
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
          //http://oeis.org/A006882
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
              Div(FunRef(Left(FuncName("choose_k_n-k_product")), Sum(Var("k"), Const(1))),
                FunRef(Left(FuncName("!")), Minus(Minus(Var("n"), Var("k")), Const(0))))
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
          FunDef(FuncName("larger_power_of_2"),
            //@formatter:off
            Var("n") :: Nil,
                IfElse(Less(Var("n"), Const(1 <<  0    )), Const(1 <<  0),
                IfElse(Less(Var("n"), Const(1 <<  0 + 1)), Const(1 <<  1),
                IfElse(Less(Var("n"), Const(1 <<  1 + 1)), Const(1 <<  2),
                IfElse(Less(Var("n"), Const(1 <<  2 + 1)), Const(1 <<  3),
                IfElse(Less(Var("n"), Const(1 <<  3 + 1)), Const(1 <<  4),
                IfElse(Less(Var("n"), Const(1 <<  4 + 1)), Const(1 <<  5),
                IfElse(Less(Var("n"), Const(1 <<  5 + 1)), Const(1 <<  6),
                IfElse(Less(Var("n"), Const(1 <<  6 + 1)), Const(1 <<  7),
                IfElse(Less(Var("n"), Const(1 <<  7 + 1)), Const(1 <<  8),
                IfElse(Less(Var("n"), Const(1 <<  8 + 1)), Const(1 <<  9),
                IfElse(Less(Var("n"), Const(1 <<  9 + 1)), Const(1 << 10),
                IfElse(Less(Var("n"), Const(1 << 10 + 1)), Const(1 << 11),
                IfElse(Less(Var("n"), Const(1 << 11 + 1)), Const(1 << 12),
                IfElse(Less(Var("n"), Const(1 << 12 + 1)), Const(1 << 13),
                IfElse(Less(Var("n"), Const(1 << 13 + 1)), Const(1 << 14),
                IfElse(Less(Var("n"), Const(1 << 14 + 1)), Const(1 << 15),
                IfElse(Less(Var("n"), Const(1 << 15 + 1)), Const(1 << 16),
                IfElse(Less(Var("n"), Const(1 << 16 + 1)), Const(1 << 17),
                IfElse(Less(Var("n"), Const(1 << 17 + 1)), Const(1 << 18),
                IfElse(Less(Var("n"), Const(1 << 18 + 1)), Const(1 << 19),
                IfElse(Less(Var("n"), Const(1 << 19 + 1)), Const(1 << 20),
                IfElse(Less(Var("n"), Const(1 << 20 + 1)), Const(1 << 21),
                IfElse(Less(Var("n"), Const(1 << 21 + 1)), Const(1 << 22),
                IfElse(Less(Var("n"), Const(1 << 22 + 1)), Const(1 << 23),
                IfElse(Less(Var("n"), Const(1 << 23 + 1)), Const(1 << 24),
                IfElse(Less(Var("n"), Const(1 << 24 + 1)), Const(1 << 25),
                IfElse(Less(Var("n"), Const(1 << 25 + 1)), Const(1 << 26),
                IfElse(Less(Var("n"), Const(1 << 26 + 1)), Const(1 << 27),
                IfElse(Less(Var("n"), Const(1 << 27 + 1)), Const(1 << 28),
                IfElse(Less(Var("n"), Const(1 << 28 + 1)), Const(1 << 29),
                IfElse(Less(Var("n"), Const(1 << 29 + 1)), Const(1 << 30),
                  Const(1 << 31)
                )))))))))))))))))))))))))))))))
          ), //@formatter:on
          FunDef(FuncName("exponent_larger_power_of_2"),
            //@formatter:off
            Var("n") :: Nil,
                IfElse(Less(Var("n"), Const(1 <<  0    )), Const( 0),
                IfElse(Less(Var("n"), Const(1 <<  0 + 1)), Const( 1),
                IfElse(Less(Var("n"), Const(1 <<  1 + 1)), Const( 2),
                IfElse(Less(Var("n"), Const(1 <<  2 + 1)), Const( 3),
                IfElse(Less(Var("n"), Const(1 <<  3 + 1)), Const( 4),
                IfElse(Less(Var("n"), Const(1 <<  4 + 1)), Const( 5),
                IfElse(Less(Var("n"), Const(1 <<  5 + 1)), Const( 6),
                IfElse(Less(Var("n"), Const(1 <<  6 + 1)), Const( 7),
                IfElse(Less(Var("n"), Const(1 <<  7 + 1)), Const( 8),
                IfElse(Less(Var("n"), Const(1 <<  8 + 1)), Const( 9),
                IfElse(Less(Var("n"), Const(1 <<  9 + 1)), Const(10),
                IfElse(Less(Var("n"), Const(1 << 10 + 1)), Const(11),
                IfElse(Less(Var("n"), Const(1 << 11 + 1)), Const(12),
                IfElse(Less(Var("n"), Const(1 << 12 + 1)), Const(13),
                IfElse(Less(Var("n"), Const(1 << 13 + 1)), Const(14),
                IfElse(Less(Var("n"), Const(1 << 14 + 1)), Const(15),
                IfElse(Less(Var("n"), Const(1 << 15 + 1)), Const(16),
                IfElse(Less(Var("n"), Const(1 << 16 + 1)), Const(17),
                IfElse(Less(Var("n"), Const(1 << 17 + 1)), Const(18),
                IfElse(Less(Var("n"), Const(1 << 18 + 1)), Const(19),
                IfElse(Less(Var("n"), Const(1 << 19 + 1)), Const(20),
                IfElse(Less(Var("n"), Const(1 << 20 + 1)), Const(21),
                IfElse(Less(Var("n"), Const(1 << 21 + 1)), Const(22),
                IfElse(Less(Var("n"), Const(1 << 22 + 1)), Const(23),
                IfElse(Less(Var("n"), Const(1 << 23 + 1)), Const(24),
                IfElse(Less(Var("n"), Const(1 << 24 + 1)), Const(25),
                IfElse(Less(Var("n"), Const(1 << 25 + 1)), Const(26),
                IfElse(Less(Var("n"), Const(1 << 26 + 1)), Const(27),
                IfElse(Less(Var("n"), Const(1 << 27 + 1)), Const(28),
                IfElse(Less(Var("n"), Const(1 << 28 + 1)), Const(29),
                IfElse(Less(Var("n"), Const(1 << 29 + 1)), Const(30),
                  Const(31)
                )))))))))))))))))))))))))))))))
          ), //@formatter:on
          // http://oeis.org/A000217 triangular numbers (n*(n+1)/2)
          FunDef(FuncName("A000217"), Var("n") :: Nil, Div(Product(Var("n"), Sum(Var("n"), Const(1))), Const(2))),
          // http://oeis.org/A007318 Pascal triangle by rows
          FunDef(FuncName("A007318"), Var("n") :: Nil, FunRef(Left(FuncName("choose")),
            SmallerIndex1InAscending(Var("n"), Left(FuncName("A000217"))),
            Minus(Minus(Var("n"), Const(1)), SmallerValueInAscending(Var("n"), Left(FuncName("A000217"))))
          )),
        ).view.map(funDef => funDef.name -> funDef).toMap
      )
  }

}
