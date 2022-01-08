package com.github.aborg0.oeis.ui

import com.raquo.airstream.core.EventStream
import com.raquo.airstream.core.Signal
import com.raquo.airstream.state.Var
import org.scalajs.dom.Fetch

import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits._
import scala.util.matching.Regex

object CachedFetches {
  val OEISSequenceIdRegex: Regex = "^A\\d{6}$".r
  //%N A000045 Fibonacci numbers: F(n) = F(n-1) + F(n-2) with F(0) = 0 and F(1) = 1.
  //%C A000045 Also sometimes called Lamé's sequence.
  val DescriptionRegex: Regex = "%N A\\d{6} (.*)".r

  def fetchFromOeis(sequenceId: String): Future[String] = {
    require(OEISSequenceIdRegex.matches(sequenceId))
    // Unfortunately this does not work due to cross origin request blocking.
    Fetch
      .fetch(s"http://oeis.org/search?q=id:$sequenceId&fmt=text")
      .toFuture
      .flatMap(_.text().toFuture)
  }

  val cacheOfOeisDescriptions: Var[Map[String, Option[String]]] = Var(Map.empty)

  def descriptionOf(sequenceId: String): Signal[String] = {
    val cachedValue = cacheOfOeisDescriptions.signal.map(_.get(sequenceId))
    cachedValue.flatMap {
      case None =>
        val descriptionFullStream: EventStream[String] =
          EventStream.fromFuture(fetchFromOeis(sequenceId))
        val descriptionStream = descriptionFullStream.map(text => {
          val value = DescriptionRegex.findFirstIn(text)
          cacheOfOeisDescriptions.tryUpdate(orig =>
            orig.map(_ + (sequenceId -> value)))
          value
        })

        descriptionStream.map(_.getOrElse("")).toSignal("")
      case Some(cachedValue) =>
        EventStream.fromValue(cachedValue.getOrElse(""), true).toSignal("")
    }
  }
}
