package com.github.aborg0

import com.github.aborg0.oeis.eval.Evaluator.EvalContext
import com.github.aborg0.oeis.parser.ExpressionParser.ParseContext

package object oeis {
  def fromEvalContext(ctx: EvalContext): ParseContext =
    ParseContext(ctx.numCtx.keys.map(_.v).toSet,
                 ctx.funcCtx.keys.map(_.name).toSet)

  implicit class RangeOps(val range: Range.Inclusive) extends AnyVal {

    /**
     * Finds the first value for which `computeI` is true for `compareToLeft(compareTo, computeI(i))`,
     * where `i` is elem of `range`. {{range.end}} should be true for the comparison and all values from the first,
     * should also hold
     * @param computeI The function that can compute the `i`th element
     * @param compareTo The reference value
     * @param compareToLeft The comparison function
     * @tparam T Type of the computed values
     * @return The first index for which the `compareToLeft(compareTo, computeI(i))` is true
     */
    def binarySearchFirst[T](computeI: Int => T,
                             compareTo: T,
                             compareToLeft: (T, T) => Boolean): Int = {
      if (range.start == range.end) {
        require(compareToLeft(compareTo, computeI(range.start)),
                s"Value: ${range.start}")
        range.start
      } else {
        if (range.step == 1) {
          val middleIndex = (range.start + range.end) / 2
          if (middleIndex == range.start) {
            require(compareToLeft(compareTo, computeI(range.end)),
                    s"Value: ${range.end}")
            range.end
          } else {
            val middleValue = computeI(middleIndex)
            if (compareToLeft(compareTo, middleValue)) {
              (range.start to middleIndex)
                .binarySearchFirst(computeI, compareTo, compareToLeft)
            } else {
              (middleIndex to range.end)
                .binarySearchFirst(computeI, compareTo, compareToLeft)
            }
          }
        } else ???
      }
    }
  }
}
