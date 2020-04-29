package com.github.aborg0.oeis.parser

import com.github.aborg0.oeis.BoolExpression._
import com.github.aborg0.oeis.Expression._
import com.github.aborg0.oeis.{BoolExpression, Expression}
import fastparse.ScalaWhitespace._
import fastparse._

object ExpressionParser {
  case class ParseContext(variable: Set[String], function: Set[String])

  object ParseContext {
    val empty: ParseContext = ParseContext(Set.empty, Set.empty)
  }

  def number[_: P]: P[Const]      = P(CharIn("0-9").rep(1).!.map(_.toInt).map(Const))
  def identifier[_: P]: P[String] = P(CharIn("a-zA-Z") ~~ CharsWhileIn("a-zA-Z0-9").?).!
  def funcApply[_: P](implicit ctx: ParseContext): P[Expression] =
    P(identifier).flatMap(
      id =>
        if (ctx.variable.contains(id)) Pass(Var(id))
        else if (ctx.function.contains(id))
          P("(" ~ addSub.rep(1, ",") ~ ")").map(expr => FunRef(Left(FuncName(id)), expr:_*))
        else
          P("(" ~/ identifier.rep(1, ",", 2) ~ ")" ~ ":=").flatMap(varNames =>
            P(addSub(implicitly[P[_]], ctx.copy(function = ctx.function + id, variable = ctx.variable ++ varNames)) ~ &(End | StringIn(";", "}", "\n")))
            .map {
              case definition =>
                FunDef(FuncName(id), varNames.map(Var), definition)
          }))
  def parens[_: P](implicit ctx: ParseContext): P[Expression] =
    P("(" ~/ addSub ~ ")")
  def atom[_: P](implicit ctx: ParseContext): P[Expression] =
    P(ifElse | number | funcApply | parens | cases)
  def factorial[_: P](implicit ctx: ParseContext): P[Expression] =
    P(atom ~~ ("!!"|"!").?.!).map{
      case (expression, name@("!!" | "!")) => FunRef(Left(FuncName(name)), expression)
      case (expression, "") => expression
    }
  def factor[_: P](implicit ctx: ParseContext): P[Expression] =
    P(factorial ~ ("^" ~ factor).?).map {
      case (base, Some(exponent)) => Power(base, exponent)
      case (v, None)              => v
    }
  def divMul[_: P](implicit ctx: ParseContext): P[Expression] =
    P(factor ~ (CharIn("*/%").! ~/ factor).rep).map {
      case (l, opAndRests) =>
        opAndRests.foldLeft(l) {
          case (acc, ("*", right)) =>
            acc match {
              case Product(prods @ _*) => Product(prods :+ right: _*)
              case _                   => Product(acc, right)
            }
          case (acc, ("/", right)) => Div(acc, right)
          case (acc, ("%", right)) => Mod(acc, right)
        }
    }
  def addSub[_: P](implicit ctx: ParseContext): P[Expression] =
    P("-" ~ divMul).map(Minus(Const(0), _)) |
      P(divMul ~ (CharIn("+\\-").! ~/ divMul).rep).map {
        case (l, opAndRests) =>
          opAndRests.foldLeft(l) {
            case (acc, ("+", right)) =>
              acc match {
                case Sum(parts @ _*) => Sum(parts :+ right: _*)
                case _               => Sum(acc, right)
              }
            case (acc, ("-", right)) => Minus(acc, right)
          }
      }

  def boolConst[_: P](implicit ctx: ParseContext): P[BoolExpression] =
    P("true".! | "false".!).map {
      case "true"  => True
      case "false" => False
    }
  def relation[_: P](implicit ctx: ParseContext): P[BoolExpression] =
    boolConst | P(
      addSub ~ StringIn("<", "<=", ">", ">=", "=", "==").! ~/ addSub).map {
      case (left, "<", right)        => Less(left, right)
      case (left, "<=", right)       => LessOrEqual(left, right)
      case (left, ">", right)        => Greater(left, right)
      case (left, ">=", right)       => GreaterOrEqual(left, right)
      case (left, "=" | "==", right) => Equal(left, right)
    }
  def not[_: P](implicit ctx: ParseContext): P[BoolExpression] =
    P((StringIn("!", "not") ~/ not).map(Not) | relation)
  def and[_: P](implicit ctx: ParseContext): P[BoolExpression] =
    P(not ~ (StringIn("&", "&&", "and") ~/ not).rep).map {
      case (And(lefts @ _*), rest) if rest.nonEmpty => And(lefts ++ rest: _*)
      case (left, rest) if rest.nonEmpty            => And(left +: rest: _*)
      case (left, _)                                => left
    }
  def or[_: P](implicit ctx: ParseContext): P[BoolExpression] =
    P(and ~ (StringIn("|", "||", "or") ~/ and).rep).map {
      case (Or(lefts @ _*), rest) if rest.nonEmpty => Or(lefts ++ rest: _*)
      case (left, rest) if rest.nonEmpty           => Or(left +: rest: _*)
      case (left, _)                               => left
    }
  def imply[_: P](implicit ctx: ParseContext): P[BoolExpression] =
    P(or ~ (StringIn(":-", "=>").! ~/ or).?).map {
      case (left, Some((":-", right))) => Imply(right, left)
      case (left, Some(("=>", right))) => Imply(right, left)
      case (left, None)                => left
    }
  def brackets[_: P](implicit ctx: ParseContext): P[BoolExpression] =
    P("[" ~/ brackets ~ "]" | imply)
  def boolAtom[_: P](implicit ctx: ParseContext): P[BoolExpression] =
    P(brackets)
  def ifElse[_: P](implicit ctx: ParseContext): P[Expression] =
    P("if" ~/ boolAtom ~ "then" ~ addSub ~ "else" ~ addSub ~ "fi").map {
      case (cond, trueExpr, falseExpr) => IfElse(cond, trueExpr, falseExpr)
    }
  def cases[_: P](implicit ctx: ParseContext): P[Expression] =
    P("{" ~/ (boolAtom ~ ":" ~ addSub ~ ";").rep(1) ~ ":" ~/ addSub ~ "}").map {
      case (cases, otherwise) =>
        Cases(otherwise,
              cases.map { case (cond, expr) => Case(cond, expr) }: _*)
    }
  def expr[_: P](implicit ctx: ParseContext): P[Expression] = P(addSub ~ End)

  def parseFormula(str: String)(ctx: ParseContext = ParseContext.empty): Parsed[Expression] = parse(str, expr(_, ctx))
}
