package com.github.aborg0.oeis.eval

import com.github.aborg0.oeis.BoolExpression.{PredName, PredicateDef}
import com.github.aborg0.oeis.Expression.{Const, FunDef, FunRef}
import com.github.aborg0.oeis.Relation.PointRelation
import com.github.aborg0.oeis.{BoolExpression, Expression, SetExpression}

object AstHelper {
  def collectVariablesOfSet(expression: SetExpression, found: Set[Expression.Var] = Set.empty): Set[Expression.Var] =
    expression match {
      case SetExpression.Intersect(sets@_*) => sets.foldLeft(found)((acc, set) => acc ++ collectVariablesOfSet(set, acc))
      case SetExpression.Union(sets@_*) => sets.foldLeft(found)((acc, set) => acc ++ collectVariablesOfSet(set, acc))
      case SetExpression.RestrictByPredicate(pred, set) => collectVariablesOfSet(set, found) ++ (pred match {
        case Left(PredName(name)) => ???
        case Right(PredicateDef(predName, vars, expressions@_*)) => ???
      })
      case SetExpression.CartesianProduct(sets@_*) => sets.foldLeft(found)((acc, set) => acc ++ collectVariablesOfSet(set, acc))
      case SetExpression.Enumerate(values@_*) => values.foldLeft(found)((acc, expr) => acc ++ collectVariables(expr, acc))
      case SetExpression.RangeInclusive(from, to) => collectVariables(from, found) ++ collectVariables(to, found)
    }
  def collectVariablesOfBool(expression: BoolExpression, found: Set[Expression.Var] = Set.empty): Set[Expression.Var] =
    expression match {
      case BoolExpression.True => found
      case BoolExpression.False => found
      case BoolExpression.Not(expr) => collectVariablesOfBool(expr, found)
      case BoolExpression.And(expressions@_*) => expressions.foldLeft(found)((acc, expr) => acc ++ collectVariablesOfBool(expr, acc))
      case BoolExpression.Or(expressions@_*) => expressions.foldLeft(found)((acc, expr) => acc ++ collectVariablesOfBool(expr, acc))
      case BoolExpression.Imply(antecedent, consequence) => collectVariablesOfBool(antecedent, collectVariablesOfBool(consequence, found))
      case relation: PointRelation => collectVariables(relation.left, collectVariables(relation.right, found))
      case BoolExpression.IsIn(vs, set) => collectVariablesOfSet(set, found) ++ vs.flatMap(collectVariables(_, found))
      case BoolExpression.PredicateRef(predName, point@_*) => ???
      case BoolExpression.PredicateDef(predName, vars, expressions@_*) => found ++ vars ++ ???
      case BoolExpression.ForAll(set, predicate) =>
        collectVariablesOfSet(set, found) ++ ???
      case BoolExpression.ForAll(set, predicate) => ???
    }

  def collectVariables(expression: Expression, found: Set[Expression.Var] = Set.empty): Set[Expression.Var] =
    expression match {
      case variable@Expression.Var(v) => found + variable
      case FunDef(name, variables, expression) => found ++ variables
      case Const(t) => found
      case Expression.Sum(expressions@_*) => expressions.foldLeft(found)((acc, expr) => acc ++ collectVariables(expr, acc))
      case Expression.Product(expressions@_*) => expressions.foldLeft(found)((acc, expr) => acc ++ collectVariables(expr, acc))
      case Expression.SafeProduct(expressions@_*) => expressions.foldLeft(found)((acc, expr) => acc ++ collectVariables(expr, acc))
      case Expression.Power(base, exponent) => collectVariables(base, collectVariables(exponent, found))
      case Expression.Minus(from, num) => collectVariables(from, collectVariables(num, found))
      case Expression.Div(num, denom) => collectVariables(num, collectVariables(denom, found))
      case Expression.Mod(num, denom) => collectVariables(num, collectVariables(denom, found))
      case FunRef(name, args@_*) => args.foldLeft(found)((acc, expr) => acc ++ collectVariables(expr, acc))
      case Expression.IfElse(pred, trueValue, falseValue) => collectVariablesOfBool(pred, collectVariables(trueValue, collectVariables(falseValue, found)))
      case Expression.Cases(base, cases@_*) => cases.foldLeft(collectVariables(base, found))((acc, aCase) => acc ++ collectVariables(aCase.expression, acc) ++ collectVariablesOfBool(aCase.condition, acc))
      case Expression.LargerOrEqualValueInAscending(v, reference) => collectVariables(v, found) ++ reference.fold(_ => Set.empty, _.variables)
      case Expression.LargerOrEqualIndex1InAscending(v, reference) => collectVariables(v, found) ++ reference.fold(_ => Set.empty, _.variables)
      case Expression.SmallerValueInAscending(v, reference) =>collectVariables(v, found) ++ reference.fold(_ => Set.empty, _.variables)
      case Expression.SmallerIndex1InAscending(v, reference) =>collectVariables(v, found) ++ reference.fold(_ => Set.empty, _.variables)
      case Expression.Cardinality(set) => collectVariablesOfSet(set, found)
      case Expression.Project(point, dimension) => point.foldLeft(found)((acc, p) => acc ++ collectVariables(p, acc))
    }

}
