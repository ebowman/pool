package pool

import org.scalacheck.Gen




case class Product(name: String, current: Int, growthModel: GrowthModel)

case class Head(name: String, pitchEfficiency: Double, product: Option[Product] = None)

case class Hire(name: String)

case class Ensemble(products: Seq[Product], heads: Seq[Head], hires: Seq[Hire])

object ModelFactory {

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
      current <- Gen.choose(40, 60)
      budget <- Gen.choose(1, 20)
      name <- genProdName()
      growthModel <- genGrowthModel()
    } yield Product(name, current, growthModel)
  }

  def genSmallProduct(): Gen[Product] = {
    for {
      current <- Gen.choose(0, 10)
      budget <- Gen.choose(1, 15)
      name <- genProdName()
      growthModel <- genGrowthModel()
    } yield Product(name, current, growthModel)
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
               sorter: (Head, Head) => Boolean): Gen[Ensemble] = {
    require(bigSmallRatio >= 0d && bigSmallRatio <= 1)
    val bigCount = math.round(bigSmallRatio * headCount).toInt
    val smallCount = headCount - bigCount
    val products = Gen.listOfN(headCount,
      Gen.frequency((bigCount, genBigProduct()),
        (smallCount, genSmallProduct())))
    val hires = Gen.listOfN(hireCount, genHire())
    val heads = Gen.listOfN(headCount, genHead())
    for {
      ps: List[Product] <- products
      hds: List[Head] <- heads
      hrs: List[Hire] <- hires
    } yield {
      val hh = ps.zip(hds).map {
        case (prod: Product, head: Head) => head.copy(product = Some(prod))
      }.sortWith(sorter)
      Ensemble(ps, hh, hrs)
    }
  }
}

