package pool

case class Product(name: String, budget: Budget)

case class Head(name: String, pitchEfficiency: Double, product: Option[Product] = None)

case class Hire(name: String)

case class Ensemble(products: Seq[Product], heads: Seq[Head], hires: Seq[Hire])

