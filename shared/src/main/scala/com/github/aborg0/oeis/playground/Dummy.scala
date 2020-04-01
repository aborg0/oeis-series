package com.github.aborg0.oeis.playground

import cats.implicits._
import com.github.aborg0.oeis.eval.Evaluator
import com.github.aborg0.oeis.eval.Evaluator.EvalContext
import com.github.aborg0.oeis.parser.ExpressionParser
import fastparse._

import scala.language.implicitConversions

object Dummy {
  import com.github.aborg0.oeis.BoolExpression._
  import com.github.aborg0.oeis.Expression._
  import com.github.aborg0.oeis._
  implicit def tToConst(t: T): Expression = Const(t)

  private def valueAndAst(
      inputs: Seq[String],
      ctx: EvalContext = EvalContext(Map.empty, Map.empty)) /*(
      expr: P[_] => P[Expression] = ExpressionParser.expr(_, fromEvalContext(ctx))
  )*/: (Seq[(String, Int)], Seq[(Option[T], Expression)]) = {
    val (parsed, failed) = inputs
      .foldLeft((Seq.empty[Either[String, Expression]], fromEvalContext(ctx))) {
        case ((acc, currCtx), input) =>
          val parsedAst: Parsed[Expression] =
            parse(input, ExpressionParser.expr(_, currCtx))
          (parsedAst match {
            case Parsed.Success(expr, _) =>
              (acc :+ Right[String, Expression](expr), expr match {
                case FunDef(name, _, _) =>
                  currCtx.copy(function = currCtx.function + name.name)
                case _ => currCtx
              })
            case failure: Parsed.Failure =>
              (acc :+ Left[String, Expression](failure.trace(true).longMsg), currCtx)
          })
      }._1
      .zipWithIndex
      .partition(_._1.isRight)
    failed.collect { case (Left(str), idx) => str -> idx } -> {
      val asts = parsed.collect { case (Right(ast), _) => ast }
      Evaluator.evaluate(asts, ctx)._1.zip(asts)
    }
//        .map(ast => (Evaluator.evaluate(ast, ctx), ast))
  }

  private def printValueAndAst(inputs: Seq[String],
                               ctx: EvalContext =
                                 EvalContext(Map.empty, Map.empty))/*(
      expr: P[_] => P[Expression] =
        ExpressionParser.expr(_, fromEvalContext(ctx)))*/: Unit = {
    println(inputs)
    val (errors, results) = valueAndAst(inputs, ctx)//(expr)
    println(s"$errors   ${results.map { case (v, ast) => (s"$v ", ast) }}")
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

    printValueAndAst(Seq("2+3*4"))//()
    printValueAndAst(Seq("2+3+1"))//()
    printValueAndAst(
      Seq("fib(n) := {n = 0: 0; n = 1: 1; : fib(n-1) + fib(n-2)}", "fib(11)"))//()
    printValueAndAst(
      Seq("fib(n) := if n = 0 then 0 else if n = 1 then 1 else fib(n-1) + fib(n-2) fi fi",
          "fib(10)"))//()
    printValueAndAst(Seq("fib(n+1)"),
                     ctx = ctxWithFib.copy(numCtx = Map(Var("n") -> 3)))//()
    printValueAndAst(Seq("fib(7)"), ctx = ctxWithFib)//()
    printValueAndAst(Seq("2^fib(7)"), ctx = ctxWithFib)//()
    printValueAndAst(Seq("if true | false | 3>2 then 44 else 2 fi"))//()
    printValueAndAst(
      Seq("{2>3: 33; 5 = 3: 22; true: {2< 0: 1111;: 2}; true: 1; : 11}"))//()
    printValueAndAst(
      Seq("sign(x) := {x > 0: 1; x = 0: 0; : -1}", "sign(2)", "sign(-2)"))//()
  }
}
