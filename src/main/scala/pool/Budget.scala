package pool

case class Budget(current: Int, model: GrowthModel, month: Int = 0, carryOver: Int = 0) {
  def evolve(hired: Int): Budget =
    copy(current = current + hired, model.evolve, month = month + 1, carryOver = carryOver + model.next() - hired)

  def toHire: Int = carryOver + model.next()
}

sealed trait GrowthModel {
  def next(): Int

  def evolve: GrowthModel

  def totalBudget(months: Int = 12): Int
}

case class LinearModel(hiresPerMonth: Int) extends GrowthModel {

  override def next() = hiresPerMonth

  override def evolve = this

  override def totalBudget(months: Int = 12) = hiresPerMonth * months
}

case class FrontLoadModel(budget: Int, months: Int = 1, curMonth: Int = 0) extends GrowthModel {

  override def next() = if (curMonth < months) budget else 0

  override def evolve = copy(curMonth = curMonth + 1)

  override def totalBudget(months: Int = 12) = math.min(months, this.months) * budget
}

case class BackLoadModel(budget: Int, months: Int = 1, curMonth: Int = 0) extends GrowthModel {

  override def next() = if (curMonth < 12 - months) 0 else budget

  override def evolve = copy(curMonth = curMonth + 1)

  override def totalBudget(months: Int = 12) = (this.months - (12 - math.min(months, 12))) * budget
}
