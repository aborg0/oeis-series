package com.github.aborg0.oeis.ui

import com.github.aborg0.oeis.Expression.{Const, FunDef, FunRef, FuncName}
import com.github.aborg0.oeis.eval.AstHelper._
import com.github.aborg0.oeis.eval.Evaluator.EvalContext
import com.github.aborg0.oeis.eval.EvaluatorMemo
import com.github.aborg0.oeis.parser.ExpressionParser
import com.github.aborg0.oeis.parser.ExpressionParser.ParseContext
import com.github.aborg0.oeis.ui.ChartJsHelpers.LabelElem
import com.raquo.laminar.api.L._
import fastparse.Parsed
import org.scalajs.dom.{document, html}
import org.scalajs.dom.raw.HTMLCanvasElement
import typings.chartJs.mod._
import typings.std.HTMLSelectElement

import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.{UndefOr, |}
import scala.scalajs.js.|._
import scala.util.Try
import scala.util.matching.Regex
import com.github.aborg0.oeis._
import EvalContext.{withSupportedFunctions => defaultCtx}
import com.raquo.laminar.nodes.ReactiveHtmlElement

object Gui {
  import CssConstants._
  private val NameRegex: Regex = "([a-zA-Z](?:[a-zA-Z0-9])*)".r

  private val sampleFormula   = "fib(n) := {n = 0: 0; n = 1: 1; : fib(n-1) + fib(n-2)}"
  private val optimistFormula = "optimist(n):=n^3-33*n^2"

  class InputBox private ( // create instances of InputBox using InputBox.apply only
      val node: Div, // consumers should add this element into the tree
      val inputNode: Input, // consumers can subscribe to events coming from this element
      val bus: EventBus[String] // source (`events`) and target (`writer`) of inputNode's events
  )

  object InputBox {
    def apply(caption: String, sizeOfTextBox: Int): InputBox = {
      val bus = new EventBus[String]()
      val inputNode: Input = input(
        typ := "text",
        value <-- bus.events,
        size := sizeOfTextBox,
        inContext(node => node.events(onInput).mapTo(node.ref.value) --> bus.writer)
      )
      val node = div(span(cls:= s"$SpaceOnRight", caption), inputNode)
      new InputBox(node, inputNode, bus)
    }
  }

  object UseFormula {
    def apply(buttonText: String, formula: String, formulaBox: InputBox): Div = {
      div(span(cls := s"formula $SpaceOnRight", formula),
        button(buttonText, onClick.mapToValue(formula) --> formulaBox.bus.writer))
    }
  }

  def main(args: Array[String]): Unit = {
    typings.chartJs.chartJsRequire

    val start = Var(1) // model for the first label (inclusive)
    val end = Var(44) // model for the last label (inclusive)

    val formulaBox = InputBox("Formula:", sizeOfTextBox = 111)

    val parsedFormulaStream: EventStream[Parsed[Expression]] = formulaBox.bus.events
      .collect {
        case name if defaultCtx.funcCtx.get(FuncName(name)).exists(_.variables.lengthIs == 1) =>
          generateFunctionByName(name)
        case formula => formula
      }
      .map(ExpressionParser.parseFormula(_)(
        fromEvalContext(defaultCtx).copy(variable = Set("n", "m", "k", "x", "y"))))
    val evaluator = EvaluatorMemo()
    lazy val chart = new ^(
      document.getElementById("innerCanvas").asInstanceOf[HTMLCanvasElement],
      ChartConfiguration(
        ChartData(
          js.Array[ChartDataSets](
            ChartDataSets(
              label = name.name,
              backgroundColor = "rgb(0, 0, 0)",
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

    def showFunDef(fun: FunDef, name: FuncName, startValue: Int, endValue: Int): Unit = {
      def labelsKeys: Range.Inclusive = startValue to endValue
      if (fun.variables.lengthIs <= 1) {
        chart.data.labels = Option(js.Array[LabelElem](labelsKeys.map(_.toString): _*)).orUndefined
        chart.data.datasets.get(0).data = Some(js.Array(labelsKeys.map { v =>
          Try(
            evaluator
              .evaluate(FunRef(Left(name), Const(v)),
                EvalContext(numCtx = Map.empty, funcCtx = defaultCtx.funcCtx ++ Map(name -> fun), predCtx = Map.empty))
              .toDouble).toOption.orUndefined: UndefOr[ChartPoint | Double | Null]
        }: _*)).orUndefined
        chart.data.datasets.get(0).label = name.name
        chart.update()
      } else {
        chart.data.datasets.get(0).data = Some(js.Array[js.UndefOr[ChartPoint | Double | Null]]()).orUndefined
        chart.update()
      }
    }

    val checkOnOeisHref = Var("")

    val supportedFunctions = div(
      span(cls:= s"$SpaceOnRight", "Supported functions"),
      select(
        cls := s"functions $SpaceOnRight",
        // No selection
        option(value := "", "") +:
          // Supported functions with single argument and compatible naming
          defaultCtx.funcCtx.toSeq
            .sortBy(_._1.name)
            .collect {
              case (FuncName(name), definition) if definition.variables.lengthIs == 1 && NameRegex.matches(name) =>
                option(value := name, name/*, title <-- CachedFetches.descriptionOf(name)*/)
            },
        {
          val values = onChange.map(_.target.asInstanceOf[HTMLSelectElement].value)
          Seq(values --> formulaBox.bus.writer, values/*.collect{
            case v if v.startsWith("A") && v.lengthIs == 7 => s"https://oeis.org/$v"
          }*/ --> checkOnOeisHref.writer)

        },
        // Replace with empty selection on change from other sources on formulaBox
        inContext(node => value <--
          formulaBox.bus.events.collect{ case formula if formula != node.ref.value => node.ref.value }.mapToValue("")
        )
      ),
      span(child <-- checkOnOeisHref.signal.map(hrefValue => if (hrefValue.isEmpty || hrefValue.lengthIs != 7) "" else
        a(href := hrefValue, target := "_blank", "Check on OEIS"))),
    )


    val content: ReactiveHtmlElement[html.Div] = div(
      formulaBox.node,
      div(
        child.text <-- parsedFormulaStream.collect {
          case Parsed.Success(value, index) => ""
          case failure: Parsed.Failure      => failure.trace().longMsg
        }
      ),
      div("Examples:",
        UseFormula("Use fib", sampleFormula, formulaBox),
        UseFormula("Use smile", optimistFormula, formulaBox),
      ),
      supportedFunctions,
      div(
        span(cls:= s"$SpaceOnRight","Left: "),
        input(cls := s"$SpaceOnRight", typ := "number", maxAttr <-- end.signal.map(_.toString),
          inContext(node => node.events(onInput).mapTo(node.ref.value.toInt) --> start.writer),
          value <-- start.signal.map(_.toString)),
        span(cls:= s"$SpaceOnRight", "Right: "),
        input(typ := "number",
          minAttr <-- start.signal.map(_.toString),
          inContext(node => node.events(onInput).mapTo(node.ref.value.toInt) --> end.writer),
          value <-- end.signal.map(_.toString)),
      ),
      canvas(idAttr := "innerCanvas"),
      child.text <-- parsedFormulaStream.startWith(ExpressionParser.parseFormula("")(ParseContext.empty))
        .combineWith(start.signal).combineWith(end.signal).map {

        case ((Parsed.Success(fun @ FunDef(name, variables, expression), index), startValue), endValue) =>
          showFunDef(fun, name, startValue, endValue)
          ""
        case ((Parsed.Success(expr, index), startValue), endValue) if collectVariables(expr).sizeIs <= 1 =>
          showFunDef(FunDef(FuncName("f"), collectVariables(expr).toSeq, expr), FuncName("f"), startValue, endValue)
          ""
        case _ =>
          chart.data.datasets.get(0).data = Some(js.Array[UndefOr[ChartPoint | Double | Null]]()).orUndefined
          chart.update()
          ""
      }
    )

    render(document.getElementById("main"), content)
  }

  private def generateFunctionByName(name: String): String = {
    if (!defaultCtx.funcCtx.contains(FuncName(name.toLowerCase))) {
      s"${name.toLowerCase}(n) := $name(n)"
    } else if (!defaultCtx.funcCtx.contains(FuncName(name.toUpperCase()))) {
      s"${name.toUpperCase}(n) := $name(n)"
    } else {
      s"f(n) := $name(n)"
    }
  }
}
