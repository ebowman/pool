package pool

import org.scalatest.{Matchers, FunSpec}

class GrowthModelSpec extends FunSpec with Matchers {

  describe("the growth models") {
    describe("the linear model") {
      it("should always return the same growth per month") {
        val model = new LinearModel(hiresPerMonth = 12)
        model.next() shouldEqual 12
        model.evolve.next() shouldEqual 12
      }
      it("should correctly describe its total budget") {
        val model = new LinearModel(hiresPerMonth = 10)
        model.totalBudget() shouldEqual 120
        model.totalBudget(10) shouldEqual 100
      }
    }
    describe("the front load model") {
      it("should properly frontload") {
        val model = new FrontLoadModel(budget = 5, months = 1)
        model.next() shouldEqual 5
        model.evolve.next() shouldEqual 0
      }
      it("should correctly describe its total budget") {
        val model = new FrontLoadModel(budget = 5, months = 2)
        model.totalBudget() shouldEqual 10
        model.totalBudget(1) shouldEqual 5
      }
    }
    describe("the back load model") {
      it("should properly backload") {
        val model = new BackLoadModel(budget = 5, months = 11)
        model.next() shouldEqual 0
        model.evolve.next() shouldEqual 5
      }
      it("should correctly describe its total budget") {
        val model = new BackLoadModel(budget = 5, months = 2)
        model.totalBudget() shouldEqual 10
        model.totalBudget(12) shouldEqual 10
        model.totalBudget(11) shouldEqual 5
        model.totalBudget(10) shouldEqual 0
      }
    }
  }
}
