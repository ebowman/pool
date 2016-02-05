package pool

import org.scalatest.{Matchers, FunSpec}

class DriverSpec extends FunSpec with Matchers {

  import Driver.{absoluteSort, relativeSort}

  describe("the sort functions") {
    describe("absolute sort") {
      it("should not reorder equivalent budgets") {
        val budgetA = Budget(0, LinearModel(5))
        val budgetB = Budget(3, LinearModel(5))
        absoluteSort(budgetA, budgetB) shouldBe false
        absoluteSort(budgetB, budgetA) shouldBe false
      }
      it("should prioritize one budget's absolute need over another's") {
        val budgetA = Budget(0, LinearModel(6))
        val budgetB = Budget(3, LinearModel(5))
        absoluteSort(budgetA, budgetB) shouldBe true
        absoluteSort(budgetB, budgetA) shouldBe false
      }
    }
  }
  describe("the relative sort") {
    it("should not reorder equivalent budgets") {
      val budgetA = Budget(10, LinearModel(50))
      val budgetB = Budget(1, LinearModel(5))
      relativeSort(budgetA, budgetB) shouldBe false
      relativeSort(budgetB, budgetA) shouldBe false
    }
    it("should not reorder a different equivalent case") {
      val budgetA = Budget(1, LinearModel(6))
      val budgetB = Budget(3, LinearModel(18))
      relativeSort(budgetA, budgetB) shouldBe false
      relativeSort(budgetB, budgetA) shouldBe false
    }
    it("should prioritize one budget's relative need over another's") {
      val budgetA = Budget(1, LinearModel(6))
      val budgetB = Budget(3, LinearModel(17))
      relativeSort(budgetA, budgetB) shouldBe true
      relativeSort(budgetB, budgetA) shouldBe false
    }
  }
}
