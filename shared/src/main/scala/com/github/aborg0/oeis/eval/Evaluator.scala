package com.github.aborg0.oeis.eval

import com.github.aborg0.oeis.BoolExpression._
import com.github.aborg0.oeis.Expression._
import com.github.aborg0.oeis.SetExpression.{RangeInclusive, RestrictByPredicate}
import com.github.aborg0.oeis.{BoolExpression, Expression, SetExpression}

abstract class Evaluator {
  import Evaluator._
  // Possibly non-thread-safe
  final def evaluate(
      expressions: Seq[Expression],
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
      case SafeProduct(expressions @ _*) =>
        expressions
          .map(evaluate(_, ctx))
          .foldLeft(1)((acc, v) => safeMult(acc, v))
      case Power(base, exponent) =>
        val b   = evaluate(base, ctx)
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
      case Div(num, denom)  => evaluate(num, ctx) / evaluate(denom, ctx)
      case Mod(num, denom)  => evaluate(num, ctx) % evaluate(denom, ctx)
      case FunRef(func, args @ _*) =>
        evaluateFunction(ctx, func, args: _*)
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
      case Cardinality(set) =>
        set match {
          case intersect @ SetExpression.Intersect(_) =>
            enumerate(intersect, ctx).size
          case union @ SetExpression.Union(_) => enumerate(union, ctx).size
          case restricted @ SetExpression.RestrictByPredicate(_, _) =>
            val values = enumerate(restricted, ctx)
            values.size
          case SetExpression.CartesianProduct(sets @ _*) =>
            sets.foldLeft(1)((prod, set) =>
              if (prod == 0) 0 else prod * evaluate(Cardinality(set), ctx))
          case SetExpression.Enumerate(values @ _*) =>
            values.map(evaluate(_, ctx)).distinct.size
          case SetExpression.RangeInclusive(from, to) =>
            val x = evaluate(from, ctx)
            val y = evaluate(to, ctx)
            (x max y) - (x min y) + 1
        }
      case Project(point, dimension) =>
        evaluate(point(dimension.index - 1), ctx)
    }

  protected def enumerate(set: SetExpression,
                          ctx: Evaluator.EvalContext): Set[IndexedSeq[T]] =
    set match {
      case SetExpression.Intersect(sets @ _*) =>
        sets
          .foldLeft[Option[Set[IndexedSeq[T]]]](None) {
            case (None, set) => Some(enumerate(set, ctx))
            case (Some(acc), set) =>
              Some(if (acc.isEmpty) acc else acc.intersect(enumerate(set, ctx)))
          }
          .getOrElse(throw new IllegalStateException(
            "Infinite set's cardinality cannot be expressed"))
      case SetExpression.Union(sets @ _*) =>
        sets.foldLeft[Set[IndexedSeq[T]]](Set.empty) {
          case (acc, set) => acc ++ enumerate(set, ctx)
        }
      case SetExpression.RestrictByPredicate(pred, set) =>
        val (variable, expr) = extractPredicate(pred, ctx)
        val values = enumerate(set, ctx)
        values.filter(point =>
          expr.zip(point).foldLeft(true) {
            case (acc, (expr, v)) =>
              val contained = evaluate(expr,
                ctx.copy(numCtx = ctx.numCtx + (variable -> v)))
              acc && contained
        })
      case SetExpression.CartesianProduct(sets @ _*) =>
        // No embedded products, those will throw
        val setsByDimension =
          sets.foldLeft[IndexedSeq[Set[T]]](IndexedSeq.empty) {
            case (acc, set) =>
              acc :+ (if (acc.lastOption.exists(_.isEmpty)) Set.empty
                      else enumerate(set, ctx).map{case IndexedSeq(v) => v})
          }
        val setsByDimensionAsSeq: IndexedSeq[Seq[T]] =
          setsByDimension.map(_.toSeq)
        if (setsByDimensionAsSeq.isEmpty || setsByDimensionAsSeq.last.isEmpty) {
          Set.empty
        } else {
          val (first, rest) = setsByDimensionAsSeq.splitAt(1)
          rest
            .foldLeft(first.head.map(IndexedSeq(_)))(
              (valuesOfPreviousDimensions, possibleValuesOfDim) =>
                for {
                  previousDimValues <- valuesOfPreviousDimensions
                  newDimValue       <- possibleValuesOfDim
                } yield previousDimValues :+ newDimValue)
            .toSet
        }
      case SetExpression.Enumerate(values @ _*) =>
        values.map(v => IndexedSeq(evaluate(v, ctx))).toSet
      case SetExpression.RangeInclusive(from, to) =>
        val x = evaluate(from, ctx)
        val y = evaluate(to, ctx)
        ((x min y) to (x max y)).map(IndexedSeq(_)).toSet
    }

  protected def extractPredicate(
      pred: Predicate,
      ctx: EvalContext): (Var, IndexedSeq[BoolExpression]) = {
    val PredicateDef(name, vars, exprs @ _*) = pred match {
      case Left(value) =>
        ctx.predCtx.getOrElse(
          value,
          throw new IllegalStateException(s"Unknown predicate: $value"))
      case Right(value) => value
    }
    require(
      vars.sizeIs == 1,
      s"Only single argument predicates can be applied on sets, but found: $vars; consider applying cartesian product first")
    require(vars.sizeIs == exprs.size,
            s"expressions should be single too: $exprs")
    vars.head -> exprs.head
  }

  import com.github.aborg0.oeis._
  protected def findIndex(compareTo: Expression,
                          reference: Either[FuncName, FunDef],
                          ctx: EvalContext,
                          compareToLeft: (T, T) => Boolean): Int = {
    val ref                                  = evaluate(compareTo, ctx)
    val FunDef(_, Seq(variable), expression) = extractFunc(reference, ctx)

    val powerIdx = Iterator
      .iterate(1)(safeMult(_, 2))
      .find(
        idx =>
          compareToLeft(
            ref,
            evaluate(expression,
                     ctx.copy(numCtx = ctx.numCtx.updated(variable, idx)))))
      .getOrElse(
        throw new IllegalStateException(s"Could not find larger value"))
    powerIdx match {
      case idx @ (1 | 2) => idx
      case _ =>
        (powerIdx / 2 to powerIdx).binarySearchFirst(
          idx =>
            evaluate(expression,
                     ctx.copy(numCtx = ctx.numCtx.updated(variable, idx))),
          ref,
          compareToLeft)
    }

  }

  protected def evaluateFunction(ctx: EvalContext,
                                 func: Either[FuncName, FunDef],
                                 args: Expression*): T = {
    val FunDef(_, variables, expression: Expression) = extractFunc(func, ctx)
    // Only works on the JVM, but can be useful there to find (catastrophic) performance regressions
    if (!Thread.interrupted()) {
      val context = variables.zip(args).foldRight(ctx) {
        case ((variable, arg), newContext) =>
          newContext.copy(
            numCtx = newContext.numCtx.updated(variable, evaluate(arg, ctx)))
      }
      evaluate(expression, context)
    } else {
      throw new IllegalStateException("Interrupted execution")
    }
  }

  protected def extractFunc(func: Either[FuncName, FunDef],
                            ctx: EvalContext): FunDef = {
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
      case BoolExpression.PredicateRef(predName, point @ _*) =>
        val (variable, expr) = extractPredicate(Left(predName), ctx)
        expr.zip(point).forall {
          case (e, v) =>
            evaluate(
              e,
              ctx.copy(numCtx = ctx.numCtx + (variable -> evaluate(v, ctx))))
        }

      case BoolExpression.PredicateDef(predName, vars, expressions @ _*) => ???
      case BoolExpression.IsIn(vs, set) =>
        enumerate(set, ctx).contains(vs.map(evaluate(_, ctx)))
      case BoolExpression.ForAll(set, predicate) =>
        val (variable, expr) = extractPredicate(predicate, ctx)
        enumerate(set, ctx).forall(point =>
          expr.zip(point).foldLeft(true) {
            case (acc, (expr, v)) =>
              acc && evaluate(expr,
                              ctx.copy(numCtx = ctx.numCtx + (variable -> v)))
        })
      case BoolExpression.Exists(set, predicate) =>
        val (variable, expr) = extractPredicate(predicate, ctx)
        val values = enumerate(set, ctx)
        values.exists(point =>
          expr.zip(point).foldLeft(true) {
            case (acc, (expr, v)) =>
              acc && evaluate(expr,
                              ctx.copy(numCtx = ctx.numCtx + (variable -> v)))
        })

    }
}

object Evaluator extends Evaluator {
  private def safeMult(a: Int, b: Int): Int = {
    val res = a.toLong * b.toLong
    if (res <= Int.MaxValue && res >= Int.MinValue) a * b
    else
      throw new IllegalStateException(s"Overflow: $a $b, res: $res")
  }

  case class EvalContext(numCtx: Map[Var, T],
                         funcCtx: Map[FuncName, FunDef],
                         predCtx: Map[PredName, PredicateDef])

  object EvalContext {
    val empty: EvalContext = EvalContext(Map.empty, Map.empty, Map.empty)
    val withSupportedFunctions: EvalContext =
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
          FunDef(
            FuncName("choose"),
            Var("n") :: Var("k") :: Nil,
            IfElse(
              Or(Less(Var("k"), Const(0)), Greater(Var("k"), Var("n"))),
              Const(0),
              IfElse(
                Less(Var("k"), Minus(Var("n"), Var("k"))),
                Div(FunRef(Left(FuncName("choose_k_n-k_product")),
                           Sum(Minus(Var("n"), Var("k")), Const(1))),
                    FunRef(Left(FuncName("!")), Minus(Var("k"), Const(0)))),
                Div(FunRef(Left(FuncName("choose_k_n-k_product")),
                           Sum(Var("k"), Const(1))),
                    FunRef(Left(FuncName("!")),
                           Minus(Minus(Var("n"), Var("k")), Const(0))))
              )
            )
          ),
          FunDef(
            FuncName("chooseRec"),
            Var("n") :: Var("k") :: Nil,
            IfElse(
              Or(Less(Var("k"), Const(0)), Greater(Var("k"), Var("n"))),
              Const(0),
              IfElse(
                Or(Equal(Var("k"), Const(0)), Equal(Var("k"), Var("n"))),
                Const(1),
                Sum(FunRef(Left(FuncName("chooseRec")),
                           Minus(Var("n"), Const(1)), Minus(Var("k"), Const(1))),
                  FunRef(Left(FuncName("chooseRec")),
                    Minus(Var("n"), Const(1)), Var("k")))
              )
            )
          ),
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
          FunDef(FuncName("A000217"),
                 Var("n") :: Nil,
                 Div(Product(Var("n"), Sum(Var("n"), Const(1))), Const(2))),
          // http://oeis.org/A007318 Pascal triangle by rows
          FunDef(
            FuncName("A007318"),
            Var("n") :: Nil,
            FunRef(
              Left(FuncName("choose")),
              SmallerIndex1InAscending(Var("n"), Left(FuncName("A000217"))),
              Minus(Minus(Var("n"), Const(1)),
                    SmallerValueInAscending(Var("n"),
                                            Left(FuncName("A000217"))))
            )
          ),
          // http://oeis.org/A000079 powers of 2
          FunDef(FuncName("A000079"),
                 Var("n") :: Nil,
                 Power(Const(2), Minus(Var("n"), Const(0)))),
          // http://oeis.org/A000045 Fibonacci
          FunDef(
            FuncName("A000045"),
            List(Var("n")),
            IfElse(
              Equal(Var("n"), Const(0)),
              Const(0),
              IfElse(
                Equal(Var("n"), Const(1)),
                Const(1),
                Sum(FunRef(Left(FuncName("A000045")),
                           Minus(Var("n"), Const(1))),
                    FunRef(Left(FuncName("A000045")),
                           Minus(Var("n"), Const(2))))
              )
            )
          ),
          FunDef(
            FuncName("gcd"),
            List(Var("n"), Var("m")),
            IfElse(
              Greater(Var("m"), Var("n")),
              FunRef(Left(FuncName("gcd")), Var("m"), Var("n")),
              IfElse(Equal(Var("m"), Const(0)),
                     Var("n"),
                     FunRef(Left(FuncName("gcd")),
                            Var("m"),
                            Mod(Var("n"), Var("m"))))
            )
          ),
          FunDef(FuncName("larger_power_of_2"),
            List(Var("n")),
            SmallerValueInAscending(Var("n"),Left(FuncName("A000079")))
          ),
          FunDef(
            FuncName("betweenAdjacentSameDenominatorsCountedEach"),
            List(Var("n"), Var("m")),
            IfElse(Equal(Var("n"), Const(0)),
              Const(0),
              Cardinality(RestrictByPredicate(Right[PredName, PredicateDef](PredicateDef(PredName("unique_name_242rqwefse"), List(Var("k")),
                Vector(Exists(
                  RangeInclusive(Div(Product(Var("k"), Var("m")), Var("n")), Div(Product(Var("k"), Sum(Var("m"), Const(1))), Var("n"))),
                  Right(PredicateDef(PredName("unique_name_4345wtw34wetr"), List(Var("j")),
                    Vector(And(Less(Product(Var("k"), Var("m")), Product(Var("j"), Var("n"))), Less(Product(Var("n"), Var("j")), Product(Var("k"), Sum(Var("m"), Const(1)))))))),
                  )
              ))), RangeInclusive(Const(1), Minus(Var("n"), Const(1)))))
            )
          ),
          FunDef(
            FuncName("betweenAdjacentSameDenominatorsCountedOnce"),
            List(Var("n"), Var("m")),
            IfElse(Equal(Var("n"), Const(0)),
              Const(0),
              Cardinality(RestrictByPredicate(Right[PredName, PredicateDef](PredicateDef(PredName("unique_name_42rdfdfqwefse"), List(Var("k")),
                Vector(Exists(
                  RangeInclusive(Div(Product(Var("k"), Var("m")), Var("n")), Div(Product(Var("k"), Sum(Var("m"), Const(1))), Var("n"))),
                  Right(PredicateDef(PredName("unique_name_434wte4wetrer"), List(Var("j")),
                    Vector(And(Equal(FunRef(Left(FuncName("gcd")), Var("k"), Var("j")), Const(1)), Less(Product(Var("k"), Var("m")), Product(Var("j"), Var("n"))), Less(Product(Var("n"), Var("j")), Product(Var("k"), Sum(Var("m"), Const(1)))))))),
                  )
              ))), RangeInclusive(Const(1), Minus(Var("n"), Const(1)))))
            )
          ),
          // between adjacent same denominators counted each time they appear triangle by rows
          FunDef(
            FuncName("betweenAdjacentSameDenominatorsCountedEachTriangle"),
            Var("n") :: Nil,
            FunRef(
              Left(FuncName("betweenAdjacentSameDenominatorsCountedEach")),
              SmallerIndex1InAscending(Var("n"), Left(FuncName("A000217"))),
              Minus(Minus(Var("n"), Const(1)),
                SmallerValueInAscending(Var("n"),
                  Left(FuncName("A000217"))))
            )
          ),
          // between adjacent same denominators counted once triangle by rows
          FunDef(
            FuncName("betweenAdjacentSameDenominatorsCountedOnceTriangle"),
            Var("n") :: Nil,
            FunRef(
              Left(FuncName("betweenAdjacentSameDenominatorsCountedOnce")),
              SmallerIndex1InAscending(Var("n"), Left(FuncName("A000217"))),
              Minus(Minus(Var("n"), Const(1)),
                SmallerValueInAscending(Var("n"),
                  Left(FuncName("A000217"))))
            )
          ),
        ).view.map(funDef => funDef.name -> funDef).toMap,
        Map.empty
      )
  }

}
