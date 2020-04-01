package com.github.aborg0.oeis.playground

import com.github.aborg0.oeis.eval.Evaluator
import com.github.aborg0.oeis.eval.Evaluator.EvalContext
import com.github.aborg0.oeis.parser.ExpressionParser
import fastparse._

import scala.language.implicitConversions

object Dummy {
  import com.github.aborg0.oeis._
  import com.github.aborg0.oeis.BoolExpression._
  import com.github.aborg0.oeis.Expression._
  implicit def tToConst(t: T): Expression = Const(t)

  private def valueAndAst(input: String,
                          ctx: EvalContext = EvalContext(Map.empty, Map.empty))(
      expr: P[_] => P[Expression] = ExpressionParser.expr(_, fromEvalContext(ctx))
  ): Either[String, (T, Expression)] = {
    val parsedAst: Parsed[Expression] = parse(input, expr)
    (parsedAst match {
      case Parsed.Success(expr, _) =>
             Right[String, Expression](expr)
      case failure: Parsed.Failure => Left[String, Expression](failure.trace(true).longMsg)
    }).map(ast => (Evaluator.evaluate(ast, ctx), ast))
  }

  private def printValueAndAst(input: String,
                               ctx: EvalContext = EvalContext(Map.empty, Map.empty))(
                                expr: P[_] => P[Expression] = ExpressionParser.expr(_, fromEvalContext(ctx))): Unit = {
    println(valueAndAst(input, ctx)(expr).map{case (v, ast) => (s"$input=$v ", ast)})
  }

  def main(args: Array[String]): Unit = {
    val numCtx  = Map.empty[Var, T]
    val funcCtx = Map.empty[FuncName, FunDef]
    val ctx     = EvalContext(numCtx, funcCtx)
    println(Evaluator.evaluate(Sum(1, 3, 4), ctx))
    println(Evaluator.evaluate(Power(2, 9), ctx))
    println(Evaluator.evaluate(Power(2, 8), ctx))
    println(Evaluator.evaluate(Power(2, 7), ctx))
    println(Evaluator.evaluate(Power(2, 6), ctx))
    println(Evaluator.evaluate(Power(2, 5), ctx))
    println(Evaluator.evaluate(Power(3, 4), ctx))
    println(Evaluator.evaluate(Power(3, 3), ctx))
    println(Evaluator.evaluate(Power(3, 2), ctx))
    println(Evaluator.evaluate(Power(3, 1), ctx))
    println(Evaluator.evaluate(Power(3, 0), ctx))
    println(
      Evaluator.evaluate(
        FunRef(FuncName("plusDiv2"), 4),
        ctx.copy(
          funcCtx = funcCtx.updated(
            FuncName("plusDiv2"),
            FunDef(FuncName("plusDiv2"), Var("a"), Div(Sum(Var("a"), 2), 2))))))
    val ctxWithFib = ctx.copy(
      funcCtx = funcCtx.updated(
        FuncName("fib"),
        FunDef(
          FuncName("fib"),
          Var("a"), //https://oeis.org/A000045
          Cases(
            Sum(FunRef(FuncName("fib"), Minus(Var("a"), 1)),
                FunRef(FuncName("fib"), Minus(Var("a"), 2))),
            Case(Equal(Var("a"), 0), 0),
            Case(Equal(Var("a"), 1), 1),
            //          Case(Equal(Var("a"), 2), 1)
          )
        )
      ))
    println(Evaluator.evaluate(FunRef(FuncName("fib"), 7), ctxWithFib))

    printValueAndAst("2+3*4")()
    printValueAndAst("2+3+1")()
//    printValueAndAst("fib(n) := {n = 0: 0; n = 1: 1; : fib(n-1) + fib(n-2)}")()
//    printValueAndAst("fib(n) := if n = 0 then 0 else if n = 1 then 1 else fib(n-1) + fib(n-2) fi fi")()
    printValueAndAst("fib(n+1)", ctx = ctxWithFib.copy(numCtx = Map(Var("n")->3)))()
    printValueAndAst("fib(7)", ctx = ctxWithFib)()
    printValueAndAst("2^fib(7)", ctx = ctxWithFib)()
    printValueAndAst("if true | false | 3>2 then 44 else 2 fi")()
    printValueAndAst("{2>3: 33; 5 = 3: 22; true: {2< 0: 1111;: 2}; true: 1; : 11}")()
//    printValueAndAst("sign(x) := {x > 0: 1; x = 0: 0; : -1}")()
  }
}
