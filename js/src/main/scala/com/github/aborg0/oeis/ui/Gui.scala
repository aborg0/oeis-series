package com.github.aborg0.oeis.ui

import com.github.aborg0.oeis.Expression.{Const, FunDef, FunRef}
import com.github.aborg0.oeis.eval.EvaluatorMemo
import com.github.aborg0.oeis.eval.Evaluator.EvalContext
import com.github.aborg0.oeis.parser.ExpressionParser
import com.raquo.laminar.api.L._
import fastparse.Parsed
import org.scalajs.dom
import org.scalajs.dom.document
import org.scalajs.dom.raw.{HTMLCanvasElement, HTMLElement}
import typings.chartJs.mod._
import typings.plotlyJs.mod.{Data, Datum}

import scala.util.Try
//import typings.vegaTypings._
//import typings.vegaTypings.dataMod.Data
import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.|
import scala.scalajs.js.|._

object Gui {
  def appendPar(targetNode: dom.Node, text: String): Unit = {
    val parNode = document.createElement("p")
    parNode.textContent = text
    targetNode.appendChild(parNode)
  }

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
//      val valueVar: Var[String]
  )

  object InputBox {
    def apply(caption: String): InputBox = {
//      val valueVar = Var("")
      val bus = new EventBus[String]()
      val inputNode: Input = input(typ := "text", value <-- bus.events, inContext(node => node.events(onInput).mapTo(node.ref.value) --> bus.writer))
//      inputNode.amend(onInput --> valueVar.writer)
      val node      = div(caption, inputNode)
      new InputBox(node, inputNode/*, valueVar*/, bus)
    }
  }

  def main(args: Array[String]): Unit = {
    appendPar(document.getElementById("main"), "Hello World")
    val labelsKeys= 1 to 44


    val formulaBox = InputBox("Formula")
//    val formulaBus: EventBus[String] = new EventBus()
//    val formulaStream: EventStream[String] = formulaBus.events

//    formulaBox.inputNode.events(onInput).mapTo(formulaBox.inputNode.ref.value).addObserver(formulaBox.bus.writer)(owner = unsafeWindowOwner)//(owner = formulaBox.inputNode)

    val parsedFormulaStream = formulaBox.bus.events.map(ExpressionParser.parseFormula(_)())
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
              data =
                js.Array()
              ,
              fill = false,
            ),
            //            ChartDataSets(label = "X2",
            //                          `type` = ChartType.line,
            //                          data = js.Array(5d, 2d, 3d),
            //                          fill = false),
          ),
          labels = js.Array(labelsKeys.map(_.toString): _*)
        ),
        ChartOptions(),
        js.Array[PluginServiceRegistrationOptions](),
        `type` = ChartType.line
      )
    )

    val sampleFormula = "fib(n) := {n = 0: 0; n = 1: 1; : fib(n-1) + fib(n-2)}"
    val optimistFormula = "optimist(n):=n^3-33*n^2"
    val useFib = button("Use fib", onClick.mapToValue(sampleFormula) --> formulaBox.bus.writer)
//    useFib.events(onClick).mapToValue(sampleFormula).addObserver(formulaBox.bus.writer)(owner = unsafeWindowOwner)//formulaBox.valueVar.writer

    val formulaDiv = div(
      formulaBox.node,
      div(span(s"Example: "), span(cls := "formula", sampleFormula), span(useFib)),
      div(span(cls:= "formula", optimistFormula), span(button("Use smile", onClick.mapToValue(optimistFormula) --> formulaBox.bus.writer))),
      div(
        child.text <-- parsedFormulaStream.collect{
            case Parsed.Success(value, index) => ""
            case failure: Parsed.Failure      => failure.trace().longMsg
          }
        ),
      canvas(idAttr := "innerCanvas"),
      child.text <-- parsedFormulaStream.collect {
        case Parsed.Success(fun@FunDef(name, variable, expression), index) =>
          chart.data.datasets.get(0).data = Some(js.Array(labelsKeys.map {
            v => Try(evaluator.evaluate(FunRef(name, Const(v)),
              EvalContext(Map.empty, EvalContext.withSupportedFunctions.funcCtx ++ Map(name -> fun))).toDouble)
              .toOption.orUndefined
          :scala.scalajs.js.UndefOr[typings.chartJs.mod.ChartPoint | Double | Null]}: _*)).orUndefined
          chart.data.datasets.get(0).label = name.name
          chart.update()

          typings.plotlyJs.coreMod.
            newPlot("plotly", js.Array[Data](Data(
              x = js.Array[js.Array[Datum] | Datum](labelsKeys.map(_.toDouble):_*),
              y = js.Array[js.Array[Datum] | Datum](labelsKeys.map { v => Try(evaluator.evaluate(FunRef(name, Const(v)),
                EvalContext(Map.empty, EvalContext.withSupportedFunctions.funcCtx ++ Map(name -> fun))).toDouble.asInstanceOf[java.lang.Double])
                .toOption.orNull.asInstanceOf[String | Double | js.Date | Null] }: _*),
              mode = typings.plotlyJs.plotlyJsStrings.linesPlussignmarkers,
              `type` = typings.plotlyJs.plotlyJsStrings.scatter)))

          ""
        case _ =>
//          val ctx = document.getElementById("innerCanvas").asInstanceOf[HTMLCanvasElement].getContext("2d")
//          ctx.fillStyle="#FFFFFF"
//          ctx.fillRect(0, 0, 3000, 1000)
          chart.data.datasets.get(0).data = Some(js.Array[scala.scalajs.js.UndefOr[typings.chartJs.mod.ChartPoint | Double | Null]]()).orUndefined
          chart.update()
          ""
      }

    )

    render(document.getElementById("main"), formulaDiv)

    val content: js.Array[js.Any] = js.Array[js.Any](1, 2, 6, 4).map { v =>
      js.Dynamic.literal(category = s"cat-$v", amount = v)
    }
    val ctx = document
      .getElementById("dummyCanvas")
      .asInstanceOf[HTMLCanvasElement] //.getContext("2d")
    typings.chartJs.chartJsRequire
    typings.plotlyJs.plotlyJsRequire
//    val (_, canvasElem, _) = appendCanvasWithinDiv(document.body, "dfgs")
////    AnonChart(
//    new ^(
//      canvasElem,
//      ChartConfiguration(
//        ChartData(
//          js.Array[ChartDataSets](
//            ChartDataSets(
//              label = "X1",
//              backgroundColor = "rgb(0, 0, 0)",
////                            borderColor = "yellow",
//              `type` = ChartType.scatter,
//              data = js.Array(2d, 4.5, 6d),
//              fill = false,
//            ),
////            ChartDataSets(label = "X2",
////                          `type` = ChartType.line,
////                          data = js.Array(5d, 2d, 3d),
////                          fill = false),
//          ),
//          labels = js.Array(labelsKeys.map(_.toString):_*)
//        ),
//        ChartOptions(),
//        js.Array[PluginServiceRegistrationOptions](),
//        `type` = ChartType.line
//      )
//    )
//    )
//    val spec: specMod.Spec = js.Dynamic.literal(data = Some(js.Array[Data](Data.ValuesData("serie", content, false))).orUndefined).asInstanceOf[specMod.Spec]
//    println(spec)
//    println(typings.vegaTypings.runtimeMod.parse(spec))
//    println(typings.vega.mod.parse(spec))

//      new specMod.Spec {
////      override var data = Some(js.Array[Data](Data.ValuesData("serie", content, false))).orUndefined
//    }
//    new typings.vegaTypings.runtimeMod.View(runtimeMod.parse(spec), js.Dynamic.literal(renderer = "svg", container = "#main", hover = false)).runAsync()
  }
}
