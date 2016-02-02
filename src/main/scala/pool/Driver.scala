package pool

import scala.util.Try

object Driver extends App {
  val absoluteSort = (a: Head, b: Head) => {
    val x = a.product.get.budgeted - a.product.get.current
    val y = b.product.get.budgeted - b.product.get.current
    x > y
  }
  val relativeSort = (a: Head, b: Head) => {
    val x = Try {
      (a.product.get.budgeted - a.product.get.current) / a.product.get.budgeted
    }
    val y = Try {
      b.product.get.budgeted - b.product.get.current / b.product.get.budgeted
    }
    (x.toOption, y.toOption) match {
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
