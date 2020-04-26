package com.github.aborg0.oeis.playground

import com.github.aborg0.oeis.eval.{Evaluator, EvaluatorMemo}
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
        FunRef(Left(FuncName("plusDiv2")), 4),
        ctx.copy(
          funcCtx = funcCtx.updated(
            FuncName("plusDiv2"),
            FunDef(FuncName("plusDiv2"), Var("a") :: Nil, Div(Sum(Var("a"), 2), 2))))))
    val ctxWithFib = ctx.copy(
      funcCtx = funcCtx.updated(
        FuncName("fib"),
        FunDef(
          FuncName("fib"),
          Var("a") :: Nil, //https://oeis.org/A000045
          Cases(
            Sum(FunRef(Left(FuncName("fib")), Minus(Var("a"), 1)),
                FunRef(Left(FuncName("fib")), Minus(Var("a"), 2))),
            Case(Equal(Var("a"), 0), 0),
            Case(Equal(Var("a"), 1), 1),
            //          Case(Equal(Var("a"), 2), 1)
          )
        )
      ))
    println(Evaluator.evaluate(FunRef(Left(FuncName("fib")), 7), ctxWithFib))
    println(Evaluator.evaluate(FunRef(Left(FuncName("choose_k_n-k_product")), 7), EvalContext.withSupportedFunctions.copy(numCtx = Map(Var("n") -> 7))))
    println(Evaluator.evaluate(FunRef(Left(FuncName("larger_power_of_2")),0), EvalContext.withSupportedFunctions))
    println(Evaluator.evaluate(FunRef(Left(FuncName("choose")), 7, 0), EvalContext.withSupportedFunctions))
    println
    println(Evaluator.evaluate(FunRef(Left(FuncName("A007318")), 1), EvalContext.withSupportedFunctions))
    println(Evaluator.evaluate(FunRef(Left(FuncName("A007318")), 2), EvalContext.withSupportedFunctions))
    println(Evaluator.evaluate(FunRef(Left(FuncName("A007318")), 3), EvalContext.withSupportedFunctions))
    println(Evaluator.evaluate(FunRef(Left(FuncName("A007318")), 4), EvalContext.withSupportedFunctions))
    println(Evaluator.evaluate(FunRef(Left(FuncName("A007318")), 5), EvalContext.withSupportedFunctions))
    println(Evaluator.evaluate(FunRef(Left(FuncName("A007318")), 6), EvalContext.withSupportedFunctions))
    println(Evaluator.evaluate(FunRef(Left(FuncName("A007318")), 7), EvalContext.withSupportedFunctions))
    println(Evaluator.evaluate(FunRef(Left(FuncName("A007318")), 8), EvalContext.withSupportedFunctions))
    println(Evaluator.evaluate(FunRef(Left(FuncName("A007318")), 9), EvalContext.withSupportedFunctions))
    println(Evaluator.evaluate(FunRef(Left(FuncName("A007318")), 10), EvalContext.withSupportedFunctions))
    println(Evaluator.evaluate(FunRef(Left(FuncName("A007318")), 11), EvalContext.withSupportedFunctions))
    println

    printValueAndAst(Seq("2+3*4"))//()
    printValueAndAst(Seq("2+3+1"))//()
    printValueAndAst(
      Seq("fib(n) := {n = 0: 0; n = 1: 1; : fib(n-1) + fib(n-2)}", "fib(11)"))//()
    printValueAndAst(
      Seq("fib(n) := if n = 0 then 0 else if n = 1 then 1 else fib(n-1) + fib(n-2) fi fi",
          "fib(10)"))//()
    printValueAndAst(Seq("fib(n+1)"),
                     ctx = ctxWithFib.copy(numCtx = Map(Var("n") -> 3)))//()
    println(EvaluatorMemo(ctxWithFib).evaluate(FunRef(Left(FuncName("fib")), 44), ctxWithFib))
    printValueAndAst(Seq("fib(7)"), ctx = ctxWithFib)//()
    printValueAndAst(Seq("2^fib(7)"), ctx = ctxWithFib)//()
    printValueAndAst(Seq("if true | false | 3>2 then 44 else 2 fi"))//()
    printValueAndAst(
      Seq("{2>3: 33; 5 = 3: 22; true: {2< 0: 1111;: 2}; true: 1; : 11}"))//()
    printValueAndAst(
      Seq("sign(x) := {x > 0: 1; x = 0: 0; : -1}", "sign(2)", "sign(-2)"))//()
    printValueAndAst(Seq("1!+2!", "4!", "3!!", "(3!)!", "A000142(5)"), EvalContext.withSupportedFunctions)
    printValueAndAst(Seq("triangular(n) := A000217(n)"), EvalContext.withSupportedFunctions)
  }
}
