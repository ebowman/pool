package pool

import org.scalatest.{Matchers, FunSpec}

class BudgetSpec extends FunSpec with Matchers {

  describe("a budget") {
    describe("with a linear growth model") {
      it ("should behave predictably") {

        // hiring to budget is straightforward. A LinearModel in this case
        // just means that we can hire 5 people per month, every month
        var budget = Budget(current = 0, model = LinearModel(5))
        budget.current shouldEqual 0
        budget.toHire shouldEqual 5

        // when we hire under budget, we should have more budget next month
        budget = budget.evolve(hired = 3)
        budget.current shouldEqual 3
        budget.toHire shouldEqual 7

        // when we hire over budget, we should have less budget next month
        budget = budget.evolve(hired = 8)
        budget.current shouldEqual 11
        budget.toHire shouldEqual 4
      }
    }
    describe("with a front loaded hiring model") {
     it("should behave predictably when we hire on budget") {
       // we are allowed to hire 5 people per month for the first 3 months, then no
       // more budget
       var budget: Budget = Budget(current = 0, model = FrontLoadModel(budget = 5, months = 3))

       // month 0
       budget.current shouldEqual 0
       budget.toHire shouldEqual 5

       // month 1
       budget = budget.evolve(budget.toHire)

       budget.current shouldEqual 5
       budget.toHire shouldEqual 5

       // month 2
       budget = budget.evolve(budget.toHire)

       budget.current shouldEqual 10
       budget.toHire shouldEqual 5

       // month 3 -- can't hire anymore
       budget = budget.evolve(budget.toHire)

       budget.current shouldEqual 15
       budget.toHire shouldEqual 0

       // month 4 -- can't hire anymore
       budget = budget.evolve(budget.toHire)

       budget.current shouldEqual 15
       budget.toHire shouldEqual 0
     }
    }

  }
}
