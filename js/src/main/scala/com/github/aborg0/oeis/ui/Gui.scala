package com.github.aborg0.oeis.ui

import org.scalajs.dom
import org.scalajs.dom.document
import org.scalajs.dom.raw.{HTMLCanvasElement, HTMLElement}
import typings.chartJs.mod._
//import typings.vegaTypings._
//import typings.vegaTypings.dataMod.Data
//import typings.chartJs._

import scala.scalajs.js

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

  def main(args: Array[String]): Unit = {
    println("Hello world")
    appendPar(document.getElementById("main"), "Hello World")
    val content: js.Array[js.Any] = js.Array[js.Any](1, 2, 6, 4).map { v =>
      js.Dynamic.literal(category = s"cat-$v", amount = v)
    }
    val ctx = document
      .getElementById("dummyCanvas")
      .asInstanceOf[HTMLCanvasElement] //.getContext("2d")
    typings.chartJs.chartJsRequire
    val (div, canvas, _) = appendCanvasWithinDiv(document.body, "dfgs")
//    AnonChart(
      new ^(
        canvas,
        ChartConfiguration(
          ChartData(
            js.Array[ChartDataSets](
              ChartDataSets(label = "X1",
                            backgroundColor = "rgb(0, 0, 0)",
//                            borderColor = "yellow",
                            `type` = ChartType.scatter,
                            data = js.Array(2d, 4.5, 6d),
                fill = false,
              ),
              ChartDataSets(label = "X2",
                            `type` = ChartType.line,
                            data = js.Array(5d, 2d, 3d),
                fill = false
              ),
          ),
            labels = js.Array("1", "2", "3")
          ),
          ChartOptions(),
          js.Array[PluginServiceRegistrationOptions](),
          `type` = ChartType.line
        )
      )
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
