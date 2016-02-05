package pool

import scala.util.{Failure, Success, Try}

object Driver extends App {
  def absoluteSort(a: Budget, b: Budget) = a.toHire > b.toHire

  // this has some special logic to make sure empty budgets sort last
  def relativeSort(a: Budget, b: Budget) = (a.relativeHiringPressure, b.relativeHiringPressure) match {
    case (Success(p), Success(q)) => p > q
    case (Success(_), Failure(_)) => true
    case _ => false
  }

  val ensembleGen = Generators.ensemble(
    headCount = 10,
    bigSmallRatio = 0.1,
    hireCount = 20,
    absoluteSort)
  ensembleGen.sample.foreach { ensemble =>
    println(ensemble.heads.mkString("\n"))
    //println(ensemble.hires.mkString("\n"))
    val a = Head("A", .9, null)
    val b = Head("B", .9, null)
    val heads = Set(a, b)
    val bid = Bid(heads)
    val hire = Hire("hire")
    val results = for (x <- 1 to 100000) yield bid(hire)
    println(results.count(_.head == a))
    println(results.count(_.head == b))
  }

}
