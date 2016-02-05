package pool

import org.scalacheck.Gen

object Generators {

  def genLinearModel(): Gen[LinearModel] = Gen.chooseNum(1, 5).map(LinearModel.apply)

  def genFrontLoadModel(): Gen[FrontLoadModel] = {
    for {
      rampupMonths <- Gen.frequency((7, Gen.choose(1, 1)), (2, Gen.choose(2, 2)), (1, Gen.choose(3, 3)))
      perMonthBudget <- Gen.chooseNum(1, 5)
    } yield FrontLoadModel(perMonthBudget, rampupMonths)
  }

  def genBackLoadModel(): Gen[BackLoadModel] = {
    for {
      rampupMonths <- Gen.frequency((7, Gen.choose(1, 1)), (2, Gen.choose(2, 2)), (1, Gen.choose(3, 3)))
      perMonthBudget <- Gen.chooseNum(1, 5)
    } yield BackLoadModel(perMonthBudget, rampupMonths)
  }

  def genGrowthModel(): Gen[GrowthModel] =
    Gen.frequency((5, genLinearModel()), (2, genFrontLoadModel()), (2, genBackLoadModel()))

  def genBigBudget(): Gen[Budget] = {
    for {
      current <- Gen.choose(40, 60)
      growthModel <- genGrowthModel()
    } yield Budget(current, growthModel)
  }

  def genSmallBudget(): Gen[Budget] = {
    for {
      current <- Gen.choose(1, 15)
      growthModel <- genGrowthModel()
    } yield Budget(current, growthModel)
  }

  val prodNames: Iterator[String] = {
    val productRoots = Seq("Article", "Customer", "Availability", "Search", "Brand Solutions", "Wholesale",
      "Smart Logistics", "Data", "Fashion Store", "Advertising")
    val productModifiers = Seq("Customization", "Personalization", "Matchmaking", "Distributed")
    def core() = (for {
      mod <- productModifiers
      prod <- productRoots
    } yield s"$mod $prod").toIterator
    var itr = core()
    Stream.continually {
      if (itr.hasNext) itr.next()
      else {
        itr = core()
        itr.next()
      }
    }.toIterator
  }

  val names: Iterator[String] = {
    // like MARJORIE       0.087 55.894    244
    // from http://deron.meranda.us/data/census-derived-all-first.txt
    def core() = {
      val Rx =
        """([^ ]+).*""".r
      util.Random.shuffle(
        io.Source.fromInputStream(
          getClass.getClassLoader.getResourceAsStream("names")).getLines.collect {
          case Rx(capsName) => capsName.toLowerCase.capitalize
        }.toSeq).toIterator
    }
    var itr = core()
    Stream.continually {
      if (itr.hasNext) itr.next()
      else {
        itr = core()
        itr.next()
      }
    }.toIterator
  }

  def genBigProduct(): Gen[Product] = {
    for {
      name <- genProdName()
      budget <- Generators.genBigBudget()
    } yield Product(name, budget)
  }

  def genSmallProduct(): Gen[Product] = {
    for {
      name <- genProdName()
      budget <- Generators.genSmallBudget()
    } yield Product(name, budget)
  }

  def genProdName(): Gen[String] = Gen.oneOf(Seq(prodNames.next()))

  def genName(): Gen[String] = Gen.oneOf(Seq(names.next()))

  def genHire(): Gen[Hire] = genName().map(Hire.apply)

  def genHead(): Gen[Head] = {
    val efficiency = Gen.frequency((1, Gen.choose(0.1, 0.3)), (3, Gen.choose(0.3, 0.7)), (1, Gen.choose(0.7, 1.0)))
    for {
      eff <- efficiency
      name <- genName()
    } yield Head(name, eff)
  }

  def ensemble(headCount: Int,
               bigSmallRatio: Double,
               hireCount: Int,
               sorter: (Budget, Budget) => Boolean): Gen[Ensemble] = {
    require(bigSmallRatio >= 0d && bigSmallRatio <= 1)
    val bigCount = math.round(bigSmallRatio * headCount).toInt
    val smallCount = headCount - bigCount
    val products = Gen.listOfN(headCount,
      Gen.frequency((bigCount, genBigProduct()),
        (smallCount, genSmallProduct())))
    val hires = Gen.listOfN(hireCount, genHire())
    val heads = Gen.listOfN(headCount, genHead())
    val sortAdaptor: (Head, Head) => Boolean =
      (a: Head, b: Head) =>
        (for (p <- a.product; q <- b.product) yield
          sorter(p.budget, q.budget)).getOrElse(sys.error("Missing product $a, $b"))

    for {
      ps: List[Product] <- products
      hds: List[Head] <- heads
      hrs: List[Hire] <- hires
    } yield {
      val hh = ps.zip(hds).map {
        case (prod: Product, head: Head) => head.copy(product = Some(prod))
      }.sortWith(sortAdaptor)
      Ensemble(ps, hh, hrs)
    }
  }
}
