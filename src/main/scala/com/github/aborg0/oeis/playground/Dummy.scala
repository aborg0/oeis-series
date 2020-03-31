package com.github.aborg0.oeis.playground

import com.github.aborg0.oeis.eval.Evaluator
import com.github.aborg0.oeis.eval.Evaluator.EvalContext

import scala.language.implicitConversions

object Dummy {
  import com.github.aborg0.oeis.Expression
  import com.github.aborg0.oeis.Expression._
  import com.github.aborg0.oeis.BoolExpression._
  implicit def tToConst(t: T): Expression = Const(t)

  def main(args: Array[String]): Unit = {
    val numCtx = Map.empty[Var, T]
    val funcCtx = Map.empty[FuncName, FunDef]
    val ctx = EvalContext(numCtx, funcCtx)
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
    println(Evaluator.evaluate(FunRef(FuncName("plusDiv2"), 4), ctx.copy(funcCtx = funcCtx.updated(FuncName("plusDiv2"), FunDef(FuncName("plusDiv2"), Var("a"), Div(Sum(Var("a"), 2), 2))))))
    println(Evaluator.evaluate(FunRef(FuncName("fib"), 7), ctx.copy(funcCtx = funcCtx.updated(FuncName("fib"),
      FunDef(
        FuncName("fib"), Var("a"),//https://oeis.org/A000045
        Cases(
          Sum(FunRef(FuncName("fib"), Minus(Var("a"), 1)), FunRef(FuncName("fib"), Minus(Var("a"), 2))),
          Case(Equal(Var("a"), 0), 0),
          Case(Equal(Var("a"), 1), 1),
//          Case(Equal(Var("a"), 2), 1)
        ))))))
  }
}
