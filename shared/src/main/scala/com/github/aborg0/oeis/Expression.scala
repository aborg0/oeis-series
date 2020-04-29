package com.github.aborg0.oeis

import com.github.aborg0.oeis.BoolExpression.Predicate
import com.github.aborg0.oeis.Expression.Var
import com.github.aborg0.oeis.Relation.PointRelation
import com.github.aborg0.oeis.SetExpression.DimensionIndex

object Expression {
  type T = Int
  final case class Const(t: T)                           extends AnyVal with Expression
  final case class Var(v: String)                        extends AnyVal with Expression
  final case class Sum(expressions: Expression*)         extends Expression
  final case class Product(expressions: Expression*)     extends Expression
  final case class SafeProduct(expressions: Expression*) extends Expression
  final case class Power(base: Expression, exponent: Expression)
      extends Expression
  final case class Minus(from: Expression = Const(0), num: Expression)
      extends Expression
  final case class Div(num: Expression, denom: Expression) extends Expression
  final case class Mod(num: Expression, denom: Expression) extends Expression

  type Function = Either[FuncName, FunDef]

  final case class FunDef(name: FuncName,
                          variables: Seq[Var],
                          expression: Expression)
      extends Expression
  final case class FunRef(name: Either[FuncName, FunDef], args: Expression*)
      extends Expression
//  final case class Apply(variable: Var, value: T, expression: Expression) extends Expression
  final case class IfElse(pred: BoolExpression,
                          trueValue: Expression,
                          falseValue: Expression)
      extends Expression
  final case class Case(condition: BoolExpression, expression: Expression)
  final case class Cases(base: Expression, cases: Case*) extends Expression
  final case class LargerOrEqualValueInAscending(
      v: Expression,
      reference: Either[FuncName, FunDef])
      extends Expression
  final case class LargerOrEqualIndex1InAscending(
      v: Expression,
      reference: Either[FuncName, FunDef])
      extends Expression
  final case class SmallerValueInAscending(v: Expression,
                                           reference: Either[FuncName, FunDef])
      extends Expression
  final case class SmallerIndex1InAscending(v: Expression,
                                            reference: Either[FuncName, FunDef])
      extends Expression
  final case class Cardinality(set: SetExpression)
      extends AnyVal
      with Expression

  final case class Project(point: IndexedSeq[Expression],
                           dimension: DimensionIndex)
      extends Expression

  final case class FuncName(name: String) extends AnyVal
}

sealed trait Expression extends Any

object BoolExpression {
//  final case class Pred(name: PredName, expression: Expression) extends BoolExpression
  final case object True  extends BoolExpression
  final case object False extends BoolExpression
  final case class Not(expression: BoolExpression)
      extends AnyVal
      with BoolExpression
  final case class And(expressions: BoolExpression*) extends BoolExpression
  final case class Or(expressions: BoolExpression*)  extends BoolExpression
  final case class Imply(antecedent: BoolExpression,
                         consequence: BoolExpression)
      extends BoolExpression
  final case class Equal(left: Expression, right: Expression)
      extends PointRelation
  final case class Less(left: Expression, right: Expression)
      extends PointRelation
  final case class LessOrEqual(left: Expression, right: Expression)
      extends PointRelation
  final case class Greater(left: Expression, right: Expression)
      extends PointRelation
  final case class GreaterOrEqual(left: Expression, right: Expression)
      extends PointRelation

  type Predicate = Either[PredName, PredicateDef]

  final case class PredicateRef(predName: PredName, point: Expression*)
      extends BoolExpression

  /**
   * Defines a predicate on possibly multi-dimensional set.
   * @param predName The name of the predicate
   * @param vars The free variables in the definition, each belongs to an expression
   * @param expressions The expressions containing the free variables.
   */
  final case class PredicateDef(predName: PredName,
                                vars: Seq[Var],
                                expressions: IndexedSeq[BoolExpression]*)
      extends BoolExpression

  final case class IsIn(vs: IndexedSeq[Expression], set: SetExpression)
      extends BoolExpression
//  final case class IsSubSet(left: SetExpression, right: SetExpression)
//      extends Relation[SetExpression]

  final case class ForAll(set: SetExpression, predicate: Predicate)
      extends BoolExpression
  final case class Exists(set: SetExpression, predicate: Predicate)
      extends BoolExpression

  final case class PredName(name: String) extends AnyVal
}

sealed trait BoolExpression extends Any

object SetExpression {
  final case class Intersect[T <: SetExpression](sets: T*)
      extends AnyVal
      with SetExpression
  final case class Union[T <: SetExpression](sets: T*)
      extends AnyVal
      with SetExpression
  final case class Enumerate(values: Expression*)
      extends AnyVal
      with SingleDimSetExpression

  /**
   * [from min to, from max to]
   * @param from One side of the range
   * @param to Other side of the range
   */
  final case class RangeInclusive(from: Expression, to: Expression)
      extends SingleDimSetExpression
  final case class RestrictByPredicate(pred: Predicate, set: SetExpression)
      extends SetExpression
  final case class CartesianProduct(sets: SetExpression*)
      extends AnyVal
      with SetExpression

  final case class DimensionIndex(index: Int) extends AnyVal

  object DimensionIndex {
    final val First: DimensionIndex = DimensionIndex(1)
  }

}

sealed trait SetExpression extends Any

sealed trait SingleDimSetExpression extends Any with SetExpression

sealed trait Relation[T] extends BoolExpression {
  def left: T
  def right: T
}

object Relation {
  sealed trait PointRelation extends Relation[Expression]
  sealed trait SetRelation   extends Relation[SetExpression]
}
