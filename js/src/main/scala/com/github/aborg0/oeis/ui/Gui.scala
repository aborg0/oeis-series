package com.github.aborg0.oeis.ui

import com.github.aborg0.oeis.Expression.{Const, FunDef, FunRef, FuncName}
import com.github.aborg0.oeis.eval.AstHelper._
import com.github.aborg0.oeis.eval.Evaluator.EvalContext
import com.github.aborg0.oeis.eval.EvaluatorMemo
import com.github.aborg0.oeis.parser.ExpressionParser
import com.github.aborg0.oeis.parser.ExpressionParser.ParseContext
import com.raquo.laminar.api.L._
import fastparse.Parsed
import fastparse.Parsed.Success
import org.scalajs.dom
import org.scalajs.dom.document
import org.scalajs.dom.raw.{HTMLCanvasElement, HTMLElement}
import typings.chartJs.mod._
import typings.std.HTMLSelectElement

import scala.scalajs.js.{Date, Promise, UndefOr}
import scala.util.Try
import scala.util.matching.Regex
import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.|
import scala.scalajs.js.|._

object Gui {
  private val NameRegex: Regex = "([a-zA-Z](?:[a-zA-Z0-9])*)".r

  def appendCanvasWithinDiv(
      targetNode: dom.Node,
      id: String): (HTMLElement, HTMLCanvasElement, String) = {
    val divNode = document.createElement("div").asInstanceOf[HTMLElement]
    val canvas =
      document.createElement("canvas").asInstanceOf[HTMLCanvasElement]
    canvas.setAttribute("id", id)
    targetNode.appendChild(divNode)
    divNode.appendChild(canvas)
    (divNode, canvas, id)
  }

  class InputBox private ( // create instances of InputBox using InputBox.apply only
      val node: Div, // consumers should add this element into the tree
      val inputNode: Input, // consumers can subscribe to events coming from this element
      val bus: EventBus[String]
  )

  object InputBox {
    def apply(caption: String): InputBox = {
      val bus = new EventBus[String]()
      val inputNode: Input = input(
        typ := "text",
        value <-- bus.events,
        inContext(
          node => node.events(onInput).mapTo(node.ref.value) --> bus.writer))
      val node = div(caption, inputNode)
      new InputBox(node, inputNode, bus)
    }
  }

  def main(args: Array[String]): Unit = {
    val ChartJS = typings.chartJs.chartJsRequire

    val start = Var(1)
    val end = Var(44)

    val formulaBox = InputBox("Formula")

    import com.github.aborg0.oeis._
    val parsedFormulaStream = formulaBox.bus.events
      .collect {
        case name
            if EvalContext.withSupportedFunctions.funcCtx.contains(
              FuncName(name))
              && EvalContext.withSupportedFunctions
                .funcCtx(FuncName(name))
                .variables
                .lengthIs == 1 =>
          if (!EvalContext.withSupportedFunctions.funcCtx.contains(FuncName(name.toLowerCase))) {
            s"${name.toLowerCase}(n) := $name(n)"
          } else if (!EvalContext.withSupportedFunctions.funcCtx.contains(FuncName(name.toUpperCase()))) {
            s"${name.toUpperCase}(n) := $name(n)"
          } else {
            s"f(n) := $name(n)"
          }

        case formula => formula
      }
      .map(ExpressionParser.parseFormula(_)(
        fromEvalContext(EvalContext.withSupportedFunctions).copy(variable = Set("n", "m", "k", "x", "y"))))
      .map {
        case success @ Success(_, _) => success
        case other                   => other
      }
    val evaluator = EvaluatorMemo()
    lazy val chart = new ^(
      document.getElementById("innerCanvas").asInstanceOf[HTMLCanvasElement],
      ChartConfiguration(
        ChartData(
          js.Array[ChartDataSets](
            ChartDataSets(
              label = name.name,
              backgroundColor = "rgb(0, 0, 0)",
              //                            borderColor = "yellow",
              `type` = ChartType.scatter,
              data = js.Array(),
              fill = false,
            ),
          ),
          labels = js.Array((1 to 44).map(_.toString): _*)
        ),
        ChartOptions(),
        js.Array[PluginServiceRegistrationOptions](),
        `type` = ChartType.line
      )
    )

    val sampleFormula   = "fib(n) := {n = 0: 0; n = 1: 1; : fib(n-1) + fib(n-2)}"
    val optimistFormula = "optimist(n):=n^3-33*n^2"
    val useFib = button("Use fib", onClick.mapToValue(sampleFormula) --> formulaBox.bus.writer)

    def showFunDef(fun: FunDef, name: FuncName, startValue: Int, endValue: Int): Unit = {
      def labelsKeys: Range.Inclusive = startValue to endValue
      if (fun.variables.lengthIs <= 1) {
        chart.data.labels = Option(js.Array[String | scala.scalajs.js.Array[scala.scalajs.js.Date | Double | typings.moment.mod.Moment | String] | Double | scala.scalajs.js.Date | typings.moment.mod.Moment](labelsKeys.map(_.toString): _*)).orUndefined
        chart.data.datasets.get(0).data = Some(js.Array(labelsKeys.map { v =>
          Try(
            evaluator
              .evaluate(
                FunRef(Left(name), Const(v)),
                EvalContext(Map.empty,
                  EvalContext.withSupportedFunctions.funcCtx ++ Map(
                    name -> fun), Map.empty))
              .toDouble).toOption.orUndefined: UndefOr[
            ChartPoint | Double | Null]
        }: _*)).orUndefined
        chart.data.datasets.get(0).label = name.name
        chart.update()
      } else {
        chart.data.datasets.get(0).data = Some(
          js.Array[scala.scalajs.js.UndefOr[
            typings.chartJs.mod.ChartPoint | Double | Null]]()).orUndefined
        chart.update()
      }
    }

    val formulaDiv = div(
      formulaBox.node,
      div(span("Example: "),
          span(cls := "formula", sampleFormula),
          span(useFib)),
      div(span(cls := "formula", optimistFormula),
          span(button("Use smile", onClick.mapToValue(optimistFormula) --> formulaBox.bus.writer))),
      div(
        span("Supported functions"),
        select(
          cls := "functions",
          option(value := "", "") +:
            EvalContext.withSupportedFunctions.funcCtx.toSeq
            .sortBy(_._1.name)
            .collect {
              case (FuncName(name), definition)
                  if definition.variables.lengthIs == 1 && NameRegex.matches(
                    name) =>
                option(value := name, name)
            },
          onChange.map(_.target.asInstanceOf[HTMLSelectElement].value) --> {
            formulaBox.bus.writer
          },
          inContext(node =>
            value <-- formulaBox.bus.events.collect{ case formula if formula != node.ref.value => node.ref.value }.mapToValue("")
          )
        ),
      ),
      div(
        span("Left: "),
        input(typ := "number", maxAttr <-- end.signal.map(_.toString), inContext(node => node.events(onInput).mapTo(node.ref.value.toInt) --> start.writer), value <-- start.signal.map(_.toString)),
        span("Right: "),
        input(typ := "number", minAttr <-- start.signal.map(_.toString), inContext(node => node.events(onInput).mapTo(node.ref.value.toInt) --> end.writer), value <-- end.signal.map(_.toString)),
      ),
      div(
        child.text <-- parsedFormulaStream.collect {
          case Parsed.Success(value, index) => ""
          case failure: Parsed.Failure      => failure.trace().longMsg
        }
      ),
      canvas(idAttr := "innerCanvas"),
      child.text <-- parsedFormulaStream.startWith(ExpressionParser.parseFormula("")(ParseContext.empty)).combineWith(start.signal).combineWith(end.signal).map {

        case ((Parsed.Success(fun @ FunDef(name, variables, expression), index), startValue), endValue) =>
          showFunDef(fun, name, startValue, endValue)
          ""
        case ((Parsed.Success(expr, index), startValue), endValue) if collectVariables(expr).sizeIs <= 1 =>
          showFunDef(FunDef(FuncName("f"), collectVariables(expr).toSeq, expr), FuncName("f"), startValue, endValue)
          ""
        case _ =>
          chart.data.datasets.get(0).data = Some(
            js.Array[scala.scalajs.js.UndefOr[
              typings.chartJs.mod.ChartPoint | Double | Null]]()).orUndefined
          chart.update()
          ""
      }
    )

    render(document.getElementById("main"), formulaDiv)
  }
}
