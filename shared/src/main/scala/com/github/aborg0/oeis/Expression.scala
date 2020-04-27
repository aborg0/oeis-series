package com.github.aborg0.oeis

object Expression {
  type T = Int
  final case class Const(t: T) extends AnyVal with Expression
  final case class Var(v: String) extends AnyVal with Expression
  final case class Sum(expressions: Expression*) extends Expression
  final case class Product(expressions: Expression*) extends Expression
  final case class SafeProduct(expressions: Expression*) extends Expression
  final case class Power(base: Expression, exponent: Expression) extends Expression
  final case class Minus(from: Expression = Const(0), num: Expression) extends Expression
  final case class Div(num: Expression, denom: Expression) extends Expression
  final case class Mod(num: Expression, denom: Expression) extends Expression
  final case class FunDef(name: FuncName, variables: Seq[Var], expression: Expression) extends Expression
  final case class FunRef(name: Either[FuncName, FunDef], args: Expression*) extends Expression
//  final case class Apply(variable: Var, value: T, expression: Expression) extends Expression
  final case class IfElse(pred: BoolExpression, trueValue: Expression, falseValue: Expression) extends Expression
  final case class Case(condition: BoolExpression, expression: Expression)
  final case class Cases(base: Expression, cases: Case*) extends Expression
  final case class LargerOrEqualValueInAscending(v: Expression, reference: Either[FuncName, FunDef]) extends Expression
  final case class LargerOrEqualIndex1InAscending(v: Expression, reference: Either[FuncName, FunDef]) extends Expression
  final case class SmallerValueInAscending(v: Expression, reference: Either[FuncName, FunDef]) extends Expression
  final case class SmallerIndex1InAscending(v: Expression, reference: Either[FuncName, FunDef]) extends Expression

  final case class FuncName(name: String) extends AnyVal
}

sealed trait Expression extends Any {
}

object BoolExpression {
//  final case class Pred(name: PredName, expression: Expression) extends BoolExpression
  final case object True extends BoolExpression
  final case object False extends BoolExpression
  final case class Not(expression: BoolExpression) extends BoolExpression
  final case class And(expressions: BoolExpression*) extends BoolExpression
  final case class Or(expressions: BoolExpression*) extends BoolExpression
  final case class Imply(antecedent: BoolExpression, consequence: BoolExpression) extends BoolExpression
  final case class Equal(left: Expression, right: Expression) extends Relation
  final case class Less(left: Expression, right: Expression) extends Relation
  final case class LessOrEqual(left: Expression, right: Expression) extends Relation
  final case class Greater(left: Expression, right: Expression) extends Relation
  final case class GreaterOrEqual(left: Expression, right: Expression) extends Relation

//  final case class PredName(name: String) extends AnyVal
}

sealed trait BoolExpression extends Any {

}

sealed trait Relation extends BoolExpression {
  def left: Expression
  def right: Expression
}