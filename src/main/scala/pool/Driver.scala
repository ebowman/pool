package pool

import scala.util.Try

object Driver extends App {
  val absoluteSort = (a: Head, b: Head) => {
    (for {
      productA <- a.product
      productB <- b.product
      currentA = productA.current
      currentB = productB.current
      budgetedA = productA.growthModel.next()
      budgetedB = productB.growthModel.next()
    } yield {
      val x = budgetedA - currentA
      val y = budgetedB - currentB
      x > y
    }).getOrElse(sys.error("oops"))
  }

  val relativeSort = (a: Head, b: Head) => {
    val aMetric = (for {
      product <- a.product
      current = product.current
      budgeted = product.growthModel.next()
    } yield {
      Try((budgeted - current) / budgeted)
    }).getOrElse(sys.error("oops"))
    val bMetric = (for {
      product <- b.product
      current = product.current
      budgeted = product.growthModel.next()
    } yield {
      Try((budgeted - current) / budgeted)
    }).getOrElse(sys.error("oops"))

    (aMetric.toOption, bMetric.toOption) match {
      case (Some(p), Some(q)) => p > q
      case (Some(_), None) => true
      case (None, Some(_)) => false
      case (None, None) => false
    }
  }

  val ensembleGen = ModelFactory.ensemble(
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
