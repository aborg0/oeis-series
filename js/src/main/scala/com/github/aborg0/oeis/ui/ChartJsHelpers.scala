package com.github.aborg0.oeis.ui

import typings.moment.mod.Moment

import scala.scalajs.js
import scala.scalajs.js.|

object ChartJsHelpers {
  type LabelElem = String | js.Array[js.Date | Double | Moment | String] | Double | js.Date | Moment
}
